const { spawn } = require('child_process');
const path = require('path');

const REPO_ROOT = __dirname ? path.join(__dirname, '..') : process.cwd();
const BINARY_PATH = path.join(
  REPO_ROOT,
  'target',
  'release',
  process.platform === 'win32' ? 'texel_from_games.exe' : 'texel_from_games',
);

const shards = (() => {
  const arg = process.argv[2];
  const n = arg ? parseInt(arg, 10) : 30;
  return Number.isFinite(n) && n > 0 ? n : 30;
})();

console.log(`[run_texel_from_games] Using ${shards} shards`);
console.log(`[run_texel_from_games] Binary: ${BINARY_PATH}`);

let running = 0;
let failed = 0;

for (let i = 0; i < shards; i++) {
  const shardEnv = {
    ...process.env,
    SHARD_INDEX: String(i),
    SHARD_TOTAL: String(shards),
  };

  console.log(`[run_texel_from_games] Starting shard ${i}/${shards - 1}`);

  const child = spawn(BINARY_PATH, [], {
    cwd: REPO_ROOT,
    env: shardEnv,
    stdio: ['ignore', 'pipe', 'pipe'],
  });

  running++;

  const prefix = `[shard ${i}]`;
  if (child.stdout) {
    child.stdout.on('data', (chunk) => {
      process.stdout.write(prefix + ' ' + chunk.toString());
    });
  }
  if (child.stderr) {
    child.stderr.on('data', (chunk) => {
      process.stderr.write(prefix + ' ' + chunk.toString());
    });
  }

  child.on('error', (err) => {
    running--;
    failed++;
    console.error(
      `[run_texel_from_games] Failed to start shard ${i}: ${err && err.message ? err.message : err}`,
    );
    if (running === 0) {
      console.log(`[run_texel_from_games] All shards done. Failed: ${failed}/${shards}`);
      if (failed > 0) process.exitCode = 1;
    }
  });

  child.on('exit', (code) => {
    running--;
    if (code !== 0) {
      failed++;
      console.error(`[run_texel_from_games] Shard ${i} exited with code ${code}`);
    } else {
      console.log(`[run_texel_from_games] Shard ${i} completed successfully`);
    }

    if (running === 0) {
      console.log(`[run_texel_from_games] All shards done. Failed: ${failed}/${shards}`);
      if (failed > 0) process.exitCode = 1;
    }
  });
}