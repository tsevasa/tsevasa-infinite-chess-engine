#!/usr/bin/env node
/**
 * Texel-style tuner for EvalParams.
 *
 * - Reads Texel samples from sprt/data/texel_samples.jsonl
 * - Spawns the existing sprt.js helper to build web WASM and start the UI
 * - Uses Puppeteer to talk to the browser page, which exposes
 *   window.__texel_load_samples and window.__texel_evaluate_loss
 * - Performs simple coordinate-descent tuning over non-piece-value parameters,
 *   minimizing negative log-likelihood of game results under a logistic model.
 */

const fs = require('fs');
const path = require('path');

const SPRT_DIR = __dirname;
const ROOT_DIR = path.join(SPRT_DIR, '..');
const EVAL_FILE = path.join(ROOT_DIR, 'src', 'evaluation.rs');
const DATA_DIR = path.join(SPRT_DIR, 'data');
const DATA_FILE = path.join(DATA_DIR, 'texel_features.jsonl');
const OUTPUT_FILE = path.join(DATA_DIR, 'eval_params_tuned.json');

const TEXEL_CONFIG = {
  cpScale: 400.0,
  rounds: 2,
  minStepFraction: 0.25,
};

// Instead of a hardcoded whitelist of tunable parameters, we infer tunable
// params from the feature names present in the dataset and evaluation.rs.
// This allows new eval constants / features to be added without updating
// this script, as long as evaluation.rs exposes a matching const.

// Blacklist of parameter names that should never be tuned, even if features
// with these names exist. This includes all piece values and any other
// structural parameters you want to keep fixed.
const PARAM_BLACKLIST = new Set([
  // Piece values
  'pawn_value',
  'knight_value',
  'bishop_value',
  'rook_value',
  'queen_value',
  'king_value',
  'guard_value',
  'royal_queen_value',

  'giraffe_value',
  'camel_value',
  'zebra_value',

  'knightrider_value',
  'amazon_value',
  'hawk_value',
  'chancellor_value',
  'archbishop_value',
  'centaur_value',
  'royal_centaur_value',

  'rose_value',
  'huygen_value',
]);

// Optional per-parameter overrides for step/min/max when the generic
// heuristic is not appropriate. Keys are feature/parameter names.
const PARAM_RANGE_OVERRIDES = {
  // Example:
  // king_ring_missing_penalty: { step: 4, min: 0, max: 120 },
};

// Filled at runtime by buildTunableParams based on dataset features.
let TUNABLE_PARAMS = [];

// Utility to map parameter names (king_ring_pawn_bonus) to Rust const names (KING_RING_PAWN_BONUS).
function toConstName(name) {
  return name.toUpperCase().replace(/[^A-Z0-9]+/g, '_');
}

function extractConstInt(src, constName) {
  const re = new RegExp(`const\\s+${constName}\\s*:\\s*i32\\s*=\\s*(-?\\d+);`);
  const m = src.match(re);
  if (!m) {
    throw new Error(`Could not find const ${constName} in evaluation.rs`);
  }
  return parseInt(m[1], 10);
}

function extractPieceValue(src, pattern, label) {
  const re = new RegExp(pattern);
  const m = src.match(re);
  if (!m) {
    throw new Error(`Could not find piece value for ${label} using pattern ${pattern}`);
  }
  return parseInt(m[1], 10);
}

// Full EvalParams default used as starting point. Piece values are taken from
// get_piece_value in src/evaluation.rs, and positional constants from the
// corresponding const ...: i32 = ...; definitions. Only fields listed in
// TUNABLE_PARAMS are tuned; the rest remain fixed.
function loadDefaultsFromEvaluation() {
  const text = fs.readFileSync(EVAL_FILE, 'utf8');

  // Piece values from get_piece_value() match arms.
  const pawn_value = extractPieceValue(text, 'PieceType::Pawn\\s*=>\\s*(\\d+)', 'Pawn');
  const knight_value = extractPieceValue(text, 'PieceType::Knight\\s*=>\\s*(\\d+)', 'Knight');
  const bishop_value = extractPieceValue(text, 'PieceType::Bishop\\s*=>\\s*(\\d+)', 'Bishop');
  const rook_value = extractPieceValue(text, 'PieceType::Rook\\s*=>\\s*(\\d+)', 'Rook');
  const queen_val = extractPieceValue(
    text,
    'PieceType::Queen\\s*\\|\\s*PieceType::RoyalQueen\\s*=>\\s*(\\d+)',
    'Queen/RoyalQueen',
  );
  const king_val = extractPieceValue(
    text,
    'PieceType::King\\s*\\|\\s*PieceType::Guard\\s*=>\\s*(\\d+)',
    'King/Guard',
  );

  const camel_value = extractPieceValue(text, 'PieceType::Camel\\s*=>\\s*(\\d+)', 'Camel');
  const giraffe_value = extractPieceValue(text, 'PieceType::Giraffe\\s*=>\\s*(\\d+)', 'Giraffe');
  const zebra_value = extractPieceValue(text, 'PieceType::Zebra\\s*=>\\s*(\\d+)', 'Zebra');

  const knightrider_value = extractPieceValue(
    text,
    'PieceType::Knightrider\\s*=>\\s*(\\d+)',
    'Knightrider',
  );
  const amazon_value = extractPieceValue(text, 'PieceType::Amazon\\s*=>\\s*(\\d+)', 'Amazon');
  const hawk_value = extractPieceValue(text, 'PieceType::Hawk\\s*=>\\s*(\\d+)', 'Hawk');
  const chancellor_value = extractPieceValue(
    text,
    'PieceType::Chancellor\\s*=>\\s*(\\d+)',
    'Chancellor',
  );
  const archbishop_value = extractPieceValue(
    text,
    'PieceType::Archbishop\\s*=>\\s*(\\d+)',
    'Archbishop',
  );
  const centaur_value = extractPieceValue(text, 'PieceType::Centaur\\s*=>\\s*(\\d+)', 'Centaur');
  const royal_centaur_value = extractPieceValue(
    text,
    'PieceType::RoyalCentaur\\s*=>\\s*(\\d+)',
    'RoyalCentaur',
  );

  const rose_value = extractPieceValue(text, 'PieceType::Rose\\s*=>\\s*(\\d+)', 'Rose');
  const huygen_value = extractPieceValue(text, 'PieceType::Huygen\\s*=>\\s*(\\d+)', 'Huygen');

  const params = {
    pawn_value,
    knight_value,
    bishop_value,
    rook_value,
    queen_value: queen_val,
    king_value: king_val,
    guard_value: king_val,
    royal_queen_value: queen_val,

    giraffe_value,
    camel_value,
    zebra_value,

    knightrider_value,
    amazon_value,
    hawk_value,
    chancellor_value,
    archbishop_value,
    centaur_value,
    royal_centaur_value,

    rose_value,
    huygen_value,
  };

  return params;
}

// Infer tunable parameters from dataset feature names and evaluation.rs
// consts. For each feature f[name], if there is a corresponding
// const NAME: i32 = ... in evaluation.rs and the name is not blacklisted,
// we treat it as a tunable parameter.
function buildTunableParams(params, samples) {
  const text = fs.readFileSync(EVAL_FILE, 'utf8');

  const featureNames = new Set();
  for (const s of samples) {
    const f = s.features || {};
    for (const name of Object.keys(f)) {
      featureNames.add(name);
    }
  }

  const specs = [];

  for (const name of featureNames) {
    if (PARAM_BLACKLIST.has(name)) {
      continue;
    }

    const constName = toConstName(name);
    let defaultValue;
    try {
      defaultValue = extractConstInt(text, constName);
    } catch (e) {
      // No matching const in evaluation.rs; skip this feature.
      continue;
    }

    // Seed params with the default value from evaluation.rs so that
    // baseline eval matches the current engine.
    if (typeof params[name] !== 'number') {
      params[name] = defaultValue;
    }

    const override = PARAM_RANGE_OVERRIDES[name] || {};
    let step = override.step;
    let min = override.min;
    let max = override.max;

    if (typeof step !== 'number' || typeof min !== 'number' || typeof max !== 'number') {
      const abs = Math.abs(defaultValue) || 1;
      step = Math.max(1, Math.round(abs / 4));
      const span = abs * 4;
      if (defaultValue >= 0) {
        min = 0;
        max = defaultValue + span;
      } else {
        min = defaultValue - span;
        max = defaultValue + span;
      }
    }

    specs.push({ name, step, min, max });
  }

  // Stable order for deterministic tuning runs.
  specs.sort((a, b) => a.name.localeCompare(b.name));
  TUNABLE_PARAMS = specs;
  return specs;
}

function loadDataset() {
  if (!fs.existsSync(DATA_FILE)) {
    throw new Error('Dataset file not found: ' + DATA_FILE);
  }
  const text = fs.readFileSync(DATA_FILE, 'utf8');
  const lines = text.split(/\r?\n/);
  const samples = [];
  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed) continue;
    try {
      const obj = JSON.parse(trimmed);
      if (!obj || typeof obj.features !== 'object' || obj.features === null) continue;
      if (typeof obj.result !== 'number') continue;
      samples.push(obj);
    } catch (e) {
      // skip malformed line
    }
  }
  return samples;
}

function shuffleInPlace(arr) {
  for (let i = arr.length - 1; i > 0; i--) {
    const j = (Math.random() * (i + 1)) | 0;
    const tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
  }
}

function evaluateLoss(params, samples, cfg) {
  const cpScale = cfg.cpScale || 400.0;
  let negLL = 0;
  for (const s of samples) {
    const f = s.features || {};
    let cpScore = 0;
    for (const spec of TUNABLE_PARAMS) {
      const name = spec.name;
      const w = params[name] || 0;
      const x = f[name] || 0;
      cpScore += w * x;
    }
    const z = cpScore / cpScale;
    const p = 1 / (1 + Math.exp(-z));
    const r = s.result;
    const pClamped = Math.min(Math.max(p, 1e-6), 1 - 1e-6);
    negLL += -(r * Math.log(pClamped) + (1 - r) * Math.log(1 - pClamped));
  }
  return negLL;
}

async function tuneSingleParam(params, spec, samples, currentLoss, cfg) {
  let bestValue = params[spec.name];
  let bestLoss = currentLoss;
  let improved = false;

  const sampleCount = samples.length || 1;

  let step = spec.step;
  const minStep = Math.max(1, Math.floor(spec.step * TEXEL_CONFIG.minStepFraction));
  const maxIterations = 8;

  console.log(`[texel] Tuning ${spec.name} (start=${bestValue}, step=${step}, range=[${spec.min}, ${spec.max}])`);

  for (let iter = 0; iter < maxIterations && step >= minStep; iter++) {
    const candidates = [];
    const up = Math.min(spec.max, bestValue + step);
    const down = Math.max(spec.min, bestValue - step);
    if (up !== bestValue) candidates.push(up);
    if (down !== bestValue && down !== up) candidates.push(down);

    let foundBetter = false;

    for (const v of candidates) {
      const testParams = { ...params, [spec.name]: v };
      const loss = evaluateLoss(testParams, samples, cfg);
      if (loss + 1e-6 < bestLoss) {
        bestLoss = loss;
        bestValue = v;
        foundBetter = true;
      }
    }

    if (!foundBetter) {
      step = Math.floor(step / 2);
    } else {
      improved = true;
    }

    if (!foundBetter && step < minStep) {
      break;
    }
  }

  if (improved) {
    const avg = bestLoss / sampleCount;
    console.log(
      `[texel]   ${spec.name} improved: ${params[spec.name]} -> ${bestValue}, ` +
        `negLL=${bestLoss.toFixed(4)} (avg=${avg.toFixed(6)} per sample)`,
    );
  } else {
    console.log(`[texel]   ${spec.name} no improvement (stays at ${bestValue})`);
  }

  return { improved, value: bestValue, loss: bestLoss };
}

async function main() {
  console.log('[texel] Loading dataset from', DATA_FILE);
  const samples = loadDataset();
  if (!samples.length) {
    throw new Error('No Texel samples found; run `npm run gen-dataset` first.');
  }
  const all = samples.slice();
  shuffleInPlace(all);
  const used = all;

  console.log(`[texel] Loaded dataset: ${used.length} samples (using all for tuning)`);

  const defaultParams = loadDefaultsFromEvaluation();
  const params = { ...defaultParams };
  // Infer tunable params from dataset + evaluation.rs and seed params with
  // their default const values.
  buildTunableParams(params, used);

  if (!TUNABLE_PARAMS.length) {
    throw new Error('No tunable parameters inferred from dataset features.');
  }

  let bestLoss = evaluateLoss(params, used, TEXEL_CONFIG);
  const baselineAvg = bestLoss / used.length;
  console.log(
    `[texel] Baseline neg-log-likelihood: ${bestLoss.toFixed(4)} ` +
      `(avg=${baselineAvg.toFixed(6)} per sample)`,
  );

  for (let round = 0; round < TEXEL_CONFIG.rounds; round++) {
    console.log(`\n[texel] ===== Round ${round + 1}/${TEXEL_CONFIG.rounds} =====`);
    let roundImproved = false;

    for (const spec of TUNABLE_PARAMS) {
      const result = await tuneSingleParam(params, spec, used, bestLoss, TEXEL_CONFIG);
      if (result.improved) {
        params[spec.name] = result.value;
        bestLoss = result.loss;
        roundImproved = true;
      }
    }

    const roundAvg = bestLoss / used.length;
    console.log(
      `[texel] Round ${round + 1} complete; negLL=${bestLoss.toFixed(4)} ` +
        `(avg=${roundAvg.toFixed(6)} per sample)`,
    );

    if (!roundImproved) {
      console.log('[texel] No improvements in this round; stopping early.');
      break;
    }
  }

  if (!fs.existsSync(DATA_DIR)) {
    fs.mkdirSync(DATA_DIR, { recursive: true });
  }

  fs.writeFileSync(OUTPUT_FILE, JSON.stringify({
    params,
    negLogLikelihood: bestLoss,
    samples: used.length,
    timestamp: new Date().toISOString(),
  }, null, 2));

  console.log('[texel] Tuning complete. Wrote tuned parameters to', OUTPUT_FILE);
}

if (require.main === module) {
  main().catch((err) => {
    console.error('[texel] Fatal:', err);
    process.exit(1);
  });
}
