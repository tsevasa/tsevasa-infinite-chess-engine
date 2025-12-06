// Modular Evaluation System
//
// Design Principles:
// 1. `base` contains ALL default heuristics
// 2. Variant files in `variants/` ONLY exist if they have special logic

pub mod base;
pub mod helpers;
pub mod pieces;
pub mod variants;

use crate::board::PlayerColor;
use crate::game::GameState;
use crate::Variant;

// Re-export commonly used items
pub use base::{calculate_initial_material, get_piece_value};

#[cfg(feature = "eval_tuning")]
pub use base::{reset_eval_features, snapshot_eval_features, EvalFeatures};

/// Main evaluation entry point.
#[inline]
pub fn evaluate(game: &GameState) -> i32 {
    match game.variant {
        Some(Variant::Chess) => variants::chess::evaluate(game),
        Some(Variant::ConfinedClassical) => variants::confined_classical::evaluate(game),
        Some(Variant::Obstocean) => variants::obstocean::evaluate(game),
        // Add new variants here as they get custom evaluators
        _ => base::evaluate(game), // Default: use base for all others
    }
}

/// Fast evaluation for use - just material + basic positional
#[allow(dead_code)]
#[inline]
pub fn evaluate_fast(game: &GameState) -> i32 {
    let score = game.material_score;

    if game.turn == PlayerColor::Black {
        -score
    } else {
        score
    }
}
