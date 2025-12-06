#!/usr/bin/env node
/**
 * SPSA (Simultaneous Perturbation Stochastic Approximation) Tuner
 * 
 * This script tunes HydroChess search parameters by:
 * 1. Building the engine with search_tuning feature
 * 2. Running games between θ+ and θ- parameter configurations
 * 3. Estimating gradients from win rate differences
 * 4. Updating parameters and repeating
 * 
 * Usage:
 *   node spsa.js [options]
 * 
 * Options:
 *   --iterations <n>     Number of SPSA iterations (default: 100)
 *   --games <n>          Games per side per iteration (default: 100)
 *   --tc <ms>            Time control per move in ms (default: 100)
 *   --checkpoint <n>     Save every N iterations (default: 10)
 *   --resume <file>      Resume from checkpoint JSON file
 *   --verbose            Print detailed progress
 */

import { spawn, execSync } from 'child_process';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { readFileSync, writeFileSync, existsSync, mkdirSync, readdirSync } from 'fs';
import puppeteer from 'puppeteer';

import {
    SPSA_PARAMS,
    SPSA_HYPERPARAMS,
    getDefaultParams,
    generatePerturbation,
    applyPerturbation,
    computeGradient,
    updateParams,
    getLearningRate,
    getPerturbationSize,
    validateParams
} from './spsa_config.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const ROOT_DIR = join(__dirname, '..');

// ============================================================================
// Command Line Parsing
// ============================================================================

function parseArgs() {
    const args = process.argv.slice(2);
    const options = {
        iterations: 100,
        games: 60,           // Games per side per iteration (total = games * 2)
        tc: 200,              // 200ms per move for faster games
        checkpoint: null,    // Will calculate dynamic default if not set
        resume: null,
        autoResume: true,    // Auto-resume is now the default
        fresh: false,        // Set to true to ignore checkpoints
        verbose: false,
        concurrency: 20      // Default to 20 parallel workers
    };

    for (let i = 0; i < args.length; i++) {
        const arg = args[i];
        // Handle positional argument (resume file without --resume flag)
        if (!arg.startsWith('--') && arg.endsWith('.json')) {
            options.resume = arg;
            options.autoResume = false;  // Explicit file takes precedence
            continue;
        }
        switch (arg) {
            case '--iterations':
                options.iterations = parseInt(args[++i], 10);
                break;
            case '--games':
                options.games = parseInt(args[++i], 10);
                break;
            case '--tc':
                options.tc = parseInt(args[++i], 10);
                break;
            case '--checkpoint':
                options.checkpoint = parseInt(args[++i], 10);
                break;
            case '--resume':
                options.resume = args[++i];
                options.autoResume = false;
                break;
            case '--auto-resume':
                options.autoResume = true;
                options.fresh = false;
                break;
            case '--fresh':
                options.fresh = true;
                options.autoResume = false;
                break;
            case '--verbose':
                options.verbose = true;
                break;
            case '--concurrency':
                options.concurrency = parseInt(args[++i], 10);
                break;
            case '--help':
                console.log(`SPSA Tuner for HydroChess

Usage: node spsa.mjs [options] [checkpoint.json]

Options:
  --iterations <n>    Number of SPSA iterations (default: 100)
  --games <n>         Games per side per iteration (default: 60)
  --tc <ms>           Time control per move in ms (default: 200)
  --checkpoint <n>    Save checkpoint every N iterations (default: 5% of total)
  --resume <file>     Resume from specific checkpoint JSON file
  --fresh             Start fresh, ignore any existing checkpoints
  --concurrency <n>   Number of parallel workers (default: 20)
  --verbose           Print detailed progress

Notes:
  By default, the tuner auto-resumes from the latest checkpoint.
  Use --fresh to start a new tuning session from scratch.

Examples:
  node spsa.mjs                          # Auto-resume or fresh start
  node spsa.mjs --fresh                  # Force fresh start
  node spsa.mjs checkpoints/spsa_5.json  # Resume from specific file
`);
                process.exit(0);
        }
    }

    // Dynamic default for checkpoint: 5% of total iterations, min 1
    if (options.checkpoint === null) {
        options.checkpoint = Math.max(1, Math.floor(options.iterations * 0.05));
    }

    return options;
}

/**
 * Find the latest checkpoint file in the checkpoints directory
 */
function findLatestCheckpoint() {
    const checkpointDir = join(__dirname, 'checkpoints');
    if (!existsSync(checkpointDir)) return null;

    const files = readdirSync(checkpointDir)
        .filter(f => f.startsWith('spsa_') && f.endsWith('.json'))
        .map(f => {
            const match = f.match(/spsa_(\d+)\.json/);
            return match ? { file: f, num: parseInt(match[1], 10) } : null;
        })
        .filter(x => x !== null)
        .sort((a, b) => b.num - a.num);

    if (files.length === 0) return null;
    return join(checkpointDir, files[0].file);
}

// ============================================================================
// Build Engine with search_tuning Feature
// ============================================================================

function buildEngine() {
    console.log('🔨 Building engine with search_tuning feature...');

    try {
        execSync(
            'wasm-pack build --target web --release --out-name hydrochess --out-dir sprt/web/pkg-spsa -- --features search_tuning',
            { cwd: ROOT_DIR, stdio: 'inherit' }
        );
        console.log('✅ Build complete');
        return true;
    } catch (e) {
        console.error('❌ Build failed:', e.message);
        return false;
    }
}

// ============================================================================
// Game Runner using Puppeteer
// ============================================================================

class SPSAGameRunner {
    constructor(options) {
        this.options = options;
        this.browser = null;
        this.page = null;
    }

    async init() {
        console.log('🌐 Starting browser...');
        this.browser = await puppeteer.launch({
            headless: 'new',
            protocolTimeout: 0,
            args: [
                '--no-sandbox',
                '--disable-setuid-sandbox',
                '--allow-file-access-from-files'
            ]
        });
        this.page = await this.browser.newPage();
        this.page.setDefaultTimeout(0);

        // Forward browser console to Node console for debugging
        this.page.on('console', msg => {
            const type = msg.type();
            const text = msg.text();
            if (type === 'error') {
                console.error('  [browser error]', text);
            } else if (type === 'warning') {
                console.warn('  [browser warn]', text);
            }
            // Uncomment below for verbose logging:
            // else { console.log('  [browser]', text); }
        });

        // Log page errors
        this.page.on('pageerror', err => {
            console.error('  [page error]', err.message);
        });

        // Start local server
        const serverUrl = `file://${join(__dirname, 'web', 'spsa.html')}`;
        await this.page.goto(serverUrl, { waitUntil: 'networkidle0' });

        // Wait for the SPSA runner to be registered
        await this.page.waitForFunction(() => typeof window.runSPSAGames === 'function', { timeout: 30000 });

        console.log('✅ Browser ready');
    }

    async close() {
        if (this.browser) {
            await this.browser.close();
        }
    }

    /**
     * Run games between θ+ and θ- configurations with timeout
     * Returns { plusWins, minusWins, draws }
     */
    async runGames(thetaPlus, thetaMinus, numGames) {
        const BATCH_TIMEOUT_MS = 5 * 60 * 1000; // 5 minutes

        const gamePromise = this.page.evaluate(async (plus, minus, games, tc, concurrency) => {
            return await window.runSPSAGames(plus, minus, games, tc, concurrency);
        }, thetaPlus, thetaMinus, numGames, this.options.tc, this.options.concurrency);

        // Create timeout with ability to cancel it
        let timeoutId;
        const timeoutPromise = new Promise((resolve) => {
            timeoutId = setTimeout(() => {
                console.error('  ⚠️  Batch timeout reached (5 min), forcing completion...');
                resolve({ plusWins: 0, minusWins: 0, draws: numGames * 2, timedOut: true });
            }, BATCH_TIMEOUT_MS);
        });

        const results = await Promise.race([gamePromise, timeoutPromise]);

        // IMPORTANT: Clear the timeout to prevent it from firing later
        clearTimeout(timeoutId);

        if (results.timedOut) {
            console.log('  Restarting browser to recover from stuck state...');
            await this.close();
            await this.init();
        }

        return results;
    }
}

// ============================================================================
// SPSA Main Loop
// ============================================================================

async function runSPSA(options) {
    console.log('');
    console.log('╔══════════════════════════════════════════════════════════════╗');
    console.log('║               SPSA TUNER FOR HYDROCHESS                      ║');
    console.log('╚══════════════════════════════════════════════════════════════╝');
    console.log('');

    // Initialize parameters
    let theta = getDefaultParams();
    let startIteration = 1;
    let history = [];

    // Auto-resume from latest checkpoint (default behavior) unless --fresh
    if (!options.fresh && options.autoResume && !options.resume) {
        const latest = findLatestCheckpoint();
        if (latest) {
            options.resume = latest;
            console.log(`🔍 Auto-resume: found ${latest}`);
        } else {
            console.log('🔍 No checkpoints found, starting fresh');
        }
    } else if (options.fresh) {
        console.log('🆕 Starting fresh (--fresh flag)');
    }

    // Resume from checkpoint if specified
    if (options.resume && existsSync(options.resume)) {
        console.log(`📂 Resuming from ${options.resume}`);
        const checkpoint = JSON.parse(readFileSync(options.resume, 'utf-8'));
        theta = checkpoint.theta;
        startIteration = checkpoint.iteration + 1;
        history = checkpoint.history || [];
        console.log(`   Starting from iteration ${startIteration}`);
        if (options.verbose) {
            console.log(`   Current params:`, JSON.stringify(theta, null, 2));
        }
    }

    // Build engine
    if (!buildEngine()) {
        process.exit(1);
    }

    // Initialize game runner
    const runner = new SPSAGameRunner(options);

    try {
        await runner.init();

        // SPSA main loop
        for (let k = startIteration; k <= options.iterations; k++) {
            const startTime = Date.now();

            // Compute learning rates for this iteration
            const a_k = getLearningRate(k);
            const c_k = getPerturbationSize(k);

            // Generate perturbation vector
            const delta = generatePerturbation();

            // Create θ+ and θ- configurations
            const thetaPlus = applyPerturbation(theta, delta, c_k, +1);
            const thetaMinus = applyPerturbation(theta, delta, c_k, -1);

            console.log(`\n📊 Iteration ${k}/${options.iterations}`);
            console.log(`   a_k = ${a_k.toFixed(4)}, c_k = ${c_k.toFixed(4)}`);

            // Run games
            console.log(`🎮 Running ${options.games} games per side...`);
            const results = await runner.runGames(thetaPlus, thetaMinus, options.games);

            // Compute win rates (from θ+'s perspective)
            const totalGames = results.plusWins + results.minusWins + results.draws;
            const plusWinRate = (results.plusWins + 0.5 * results.draws) / totalGames;
            const minusWinRate = (results.minusWins + 0.5 * results.draws) / totalGames;

            // Loss = 1 - winRate (we minimize loss)
            const lossPlus = 1 - plusWinRate;
            const lossMinus = 1 - minusWinRate;

            // Compute gradient estimate
            const gradient = computeGradient(delta, c_k, [lossPlus, lossMinus]);

            // Update parameters
            const oldTheta = { ...theta };
            const newTheta = updateParams(theta, gradient, a_k);
            theta = validateParams(newTheta);

            // Count how many params changed
            let changedCount = 0;
            const changedParams = [];
            for (const key of Object.keys(theta)) {
                if (theta[key] !== oldTheta[key]) {
                    changedCount++;
                    changedParams.push(`${key}: ${oldTheta[key]} → ${theta[key]}`);
                }
            }
            console.log(`   📈 Params changed: ${changedCount}/${Object.keys(theta).length}`);

            // Record history
            const elapsed = Date.now() - startTime;
            const eloEstimate = 400 * Math.log10((plusWinRate + 0.001) / (minusWinRate + 0.001));

            history.push({
                iteration: k,
                plusWinRate,
                minusWinRate,
                eloEstimate,
                elapsed
            });

            // Print summary
            console.log(`   θ+ win rate: ${(plusWinRate * 100).toFixed(1)}%`);
            console.log(`   θ- win rate: ${(minusWinRate * 100).toFixed(1)}%`);
            console.log(`   Estimated Elo diff: ${eloEstimate.toFixed(1)}`);
            console.log(`   Time: ${(elapsed / 1000).toFixed(1)}s`);

            // Checkpoint
            if (k % options.checkpoint === 0) {
                const checkpointDir = join(__dirname, 'checkpoints');
                if (!existsSync(checkpointDir)) {
                    mkdirSync(checkpointDir, { recursive: true });
                }

                const checkpointPath = join(checkpointDir, `spsa_${k}.json`);
                writeFileSync(checkpointPath, JSON.stringify({
                    iteration: k,
                    theta,
                    history,
                    timestamp: new Date().toISOString()
                }, null, 2));

                console.log(`💾 Checkpoint saved: ${checkpointPath}`);
            }
        }

        // Final results
        console.log('');
        console.log('╔══════════════════════════════════════════════════════════════╗');
        console.log('║                    TUNING COMPLETE                          ║');
        console.log('╚══════════════════════════════════════════════════════════════╝');
        console.log('');
        console.log('📊 Final Parameters:');
        console.log(JSON.stringify(theta, null, 2));

        // Save final results
        const finalPath = join(__dirname, 'spsa_final.json');
        writeFileSync(finalPath, JSON.stringify({
            theta,
            history,
            options,
            timestamp: new Date().toISOString()
        }, null, 2));
        console.log(`\n💾 Final results saved: ${finalPath}`);

        // Generate Rust constants for copy-paste
        console.log('\n📝 Copy-paste for params.rs:\n');
        for (const [name, value] of Object.entries(theta)) {
            const constName = `DEFAULT_${name.toUpperCase()}`;
            if (SPSA_PARAMS[name]) {
                console.log(`pub const ${constName}: ${typeof value === 'number' ? (Number.isInteger(value) ? 'i32' : 'f64') : 'i32'} = ${value};`);
            }
        }

    } finally {
        await runner.close();
    }
}

// ============================================================================
// Main Entry Point
// ============================================================================

const options = parseArgs();
runSPSA(options).catch(e => {
    console.error('❌ Fatal error:', e);
    process.exit(1);
});
