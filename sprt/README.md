# HydroChess SPRT Testing Tool

Sequential Probability Ratio Test (SPRT) for comparing HydroChess engine versions using a **web-based UI**.

---

## Quick Start (Web UI)

From the `sprt/` directory:

```bash
cd sprt

# Start the web SPRT helper (builds WASM for web + starts dev server)
npm run dev
```

This will:

- Copy the existing `pkg-old` directory at the engine root into `sprt/web/pkg-old` as the **OLD** reference build
- Build a new **web** WASM directly into `sprt/web/pkg-new` using `wasm-pack build --target web --out-dir sprt/web/pkg-new`
- Start `npx serve .` in `sprt/web` on port 3000

Before running this, build your OLD reference engine once from the project root:

```bash
wasm-pack build --target web --out-dir pkg-old
```

Then open in your browser:

- **URL**: `http://localhost:3000/`

All SPRT configuration and running is done from this page.

---

## Web UI Overview

The web UI has three main areas:

1. **SPRT Configuration**
2. **SPRT Test (status + stats + logs)**
3. **Game Log + Download buttons**

### 1. SPRT Configuration

Controls in the left card:

- **Bounds Preset**: `stockfish_ltc`, `stockfish_stc`, `top30`, `top200`, `all`  
  Same presets as the original CLI tool.
- **Mode**: `Gainer` or `Non-Regression`  
  Determines whether you are proving a gain or guarding against regression.
- **Alpha / Beta**: Type I / II error rates (default 0.05 / 0.05).
- **Time Per Move (ms)**: Think time per move (default 200ms).
- **Concurrency**: Number of parallel Web Workers (games in flight).
- **Min Games (even)**: Minimum number of games before SPRT is allowed to stop.  
  Automatically rounded **up to an even number** to respect game pairing.
- **Max Games (even)**: Maximum games for the run.  
  Also forced to be even.
- **Max Moves per Game**: Move limit before a game is scored as a draw.

### 2. SPRT Test Card

The right card shows:

- **Run SPRT / Stop** buttons
- **Win / Loss / Draw counters** for the **new** engine vs the old
- **Estimated Elo difference** (with error bar)
- **Colored Status line**:
  - `PASSED (new > old)` – green
  - `FAILED (no gain)` – red
  - `INCONCLUSIVE` – yellow
  - `ABORTED` – when you hit Stop
- **SPRT Test Logs** box:
  - A line per game: result, cumulative W/L/D, Elo, LLR, and the opening in brackets, e.g.  
    `Game 12: draw (W:1 L:0 D:11) Elo≈2.3±15.4 LLR=-0.01 [e2,e4]`
  - Final summary block similar to the old `printResult`:
    - Total games, score, win rate, Elo diff ± error
    - LLR and bounds

On Stop, if at least one game finished, a **Current Results (aborted)** block is printed with the same style of
summary for the partial run.

### 3. Game Log and Downloads

The **Game Log** panel at the bottom shows a timestamped stream of messages:

- Run start line
- Per-game progress lines: `Games: N/M  W:x L:y D:z  Elo≈...±...  LLR ... in [lower, upper]`
- Final completion line or abort line

Buttons:

- **Copy Log** – copies the full Game Log text to the clipboard
- **Download Logs** – downloads the Game Log as a `.txt` file
- **Download Games (ICN)** – downloads all completed games in ICN/longconv-like format, one per block
  (button is hidden/disabled until at least one game has finished).

Each ICN game contains headers like:

```
[Event "SPRT Test Game 12"] [Site "https://www.infinitechess.org/"] [Variant "Classical"]
[UTCDate "2025.11.28"] [UTCTime "22:10:00"] [Result "1/2-1/2"] [TimeControl "-"]
...
```

- **`[Result "1-0"]`** – new engine wins
- **`[Result "0-1"]`** – new engine loses
- **`[Result "1/2-1/2"]`** – draw

Moves are logged in the same coordinate-based style used elsewhere in InfiniteChess and can be copy‑pasted into
the website tools.

---

## Engine Pairing and Openings

The web SPRT UI compares two **web-target** WASM builds:

- **Old engine**: whatever is currently in the root `pkg-old` directory (copied into `sprt/web/pkg-old`)
- **New engine**: freshly built into `sprt/web/pkg-new` by `wasm-pack build --target web --out-dir sprt/web/pkg-new`

For each game:

- Games are run in **pairs**:
  - Game 0 & 1 share the same opening
  - Game 2 & 3 share a different opening, etc.
- Within each pair:
  - **Even index** game: new engine plays **White**
  - **Odd index** game: old engine plays **White**

This ensures each opening is played once with each engine taking White, which reduces first-move bias.

### Random Opening Move

Instead of using an external opening book, the web SPRT UI:

1. Starts from the standard infinite chess position (coordinate-based).
2. Chooses a **random legal first move for White**
3. Applies this as a fixed opening for a **pair of games**.

### SPRT Termination and Even Game Counts

Because the tool uses paired games per opening:

- **`minGames` and `maxGames` are forced even** in `runSprt()`
- The SPRT decision (bounds hit / max games reached) is only evaluated after an **even number** of games
  (i.e. after a full pair has finished)

This guarantees that the test never stops halfway through a color‑reversed pair.

---

## Bounds Presets and Modes

Presets are available via the dropdowns:

| Preset           | Gainer Bounds | Non-reg Bounds | Usage                        |
|------------------|---------------|----------------|------------------------------|
| `stockfish_ltc`  | [0.5, 2.5]    | [-1.75, 0.25]  | Very strong engines (LTC)    |
| `stockfish_stc`  | [0, 2]        | [-1.75, 0.25]  | Very strong engines (STC)    |
| `top30`          | [0, 3]        | [-3, 1]        | Top 30 engines               |
| `top200`         | [0, 5]        | [-5, 0]        | Top 200 engines              |
| `all` (default)  | [0, 10]       | [-10, 0]       | All other engines            |

Modes:

- **Gainer**: Prove that the new engine is at least `elo1` better than `elo0`.
- **Non-Regression**: Show that the new engine is not worse than a negative `elo0` by more than the preset.

The web UI computes:

- Log-likelihood ratio (LLR) after each game
- Estimated Elo difference + error bar
- Final verdict based on the configured `alpha`/`beta` and preset.

---

## Directory Structure (Web Version)

```
sprt/
├── sprt.js          # Web helper: builds web WASM & starts npx serve in sprt/web
├── web/
│   ├── index.html   # Browser UI
│   ├── main.js      # SPRT logic, workers, ICN generation
│   ├── sprt-worker.js
│   ├── pkg-old/     # Copied from root/pkg (old engine)
│   └── pkg-new/     # Copied from root/pkg-new (new engine)
├── README.md        # This file
└── package.json     # Local package with npm scripts
```

## SPSA Logic Tuning

The directory also contains an SPSA (Simultaneous Perturbation Stochastic Approximation) tuner for optimizing search parameters. This tool runs self-play games to automatically tune constants like reductions, pruning margins, and history bonuses.

### Quick Start

```bash
# Run the tuner with default settings (auto-resumes if checkpoint exists)
npm run spsa
```

### Usage Guide

The tuner runs iterations of games to optimize parameters defined in `spsa_config.mjs`.

**Common Options:**

- `--games <n>`: Number of games per side per iteration. Higher = more accurate gradient but slower. (Default: 60)
- `--iterations <n>`: How many iterations to run. (Default: 100)
- `--tc <ms>`: Time control per move in milliseconds. (Default: 200)
- `--concurrency <n>`: Number of parallel workers. (Default: 20)
- `--fresh`: Ignore existing checkpoints and start tuning from scratch.
- `--verbose`: Show detailed parameter updates and debug info.

**Examples:**

```bash
# Faster run with fewer games per iteration
npm run spsa -- --games 20 --tc 25

# Start fresh with high concurrency
npm run spsa -- --fresh --concurrency 20

# Run for many iterations
npm run spsa -- --iterations 1000
```

### Checkpoints and Resuming

- Checkpoints are saved to `sprt/checkpoints/spsa_N.json`.
- By default, checkpoints are saved every **5% of total iterations** (e.g., every 5 iterations for a 100-iteration run).
- You can change this frequency with `--checkpoint <n>`.
- `npm run spsa` will **automatically find the latest checkpoint** and resume from it.
- Use `--fresh` to force a new run from default parameters.
- You can also resume from a specific file if needed: 
  ```bash
  npm run spsa -- checkpoints/spsa_50.json
  ```

### Configuration

Editable parameters and their bounds are defined in `sprt/spsa_config.mjs`. This file also controls the SPSA hyperparameters (learning rate `a`, perturbation size `c`, stability constant `A`, etc.).

---

## References

- [SPRT on Chess Programming Wiki](https://www.chessprogramming.org/Sequential_Probability_Ratio_Test)
- [OpenBench](https://github.com/AndyGrant/OpenBench) - Distributed SPRT testing
- [fast-chess](https://github.com/Disservin/fast-chess) - CLI tournament manager
