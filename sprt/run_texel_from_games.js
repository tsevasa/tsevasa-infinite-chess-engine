const { spawn } = require('child_process');
const path = require('path');

const REPO_ROOT = __dirname ? path.join(__dirname, '..') : process.cwd();
const BINARY_PATH = path.join(
  REPO_ROOT,
  'target',
  'release',
  process.platform === 'win32' ? 'texel_from_games.exe' : 'texel_from_games',
);

console.log(`[run_texel_from_games] Binary: ${BINARY_PATH}`);
console.log('[run_texel_from_games] Running single-process Texel feature extraction...');

const child = spawn(BINARY_PATH, [], {
  cwd: REPO_ROOT,
  env: { ...process.env },
  stdio: ['ignore', 'pipe', 'pipe'],
});

if (child.stdout) {
  child.stdout.on('data', (chunk) => {
    process.stdout.write(chunk.toString());
  });
}
if (child.stderr) {
  child.stderr.on('data', (chunk) => {
    process.stderr.write(chunk.toString());
  });
}

child.on('error', (err) => {
  console.error(
    `[run_texel_from_games] Failed to start texel_from_games: ${err && err.message ? err.message : err}`,
  );
  process.exitCode = 1;
});

child.on('exit', (code) => {
  if (code !== 0) {
    console.error(`[run_texel_from_games] texel_from_games exited with code ${code}`);
    process.exitCode = code ?? 1;
  } else {
    console.log('[run_texel_from_games] texel_from_games completed successfully');
  }
});