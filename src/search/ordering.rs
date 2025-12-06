use crate::evaluation::get_piece_value;
use crate::game::GameState;
use crate::moves::Move;
use crate::search::static_exchange_eval;

use super::params::{
    see_winning_threshold, sort_countermove, sort_hash, sort_killer1, sort_killer2,
    sort_winning_capture, DEFAULT_SORT_LOSING_CAPTURE, DEFAULT_SORT_QUIET,
};
use super::Searcher;

// Move ordering helpers
pub fn sort_moves(
    searcher: &Searcher,
    game: &GameState,
    moves: &mut Vec<Move>,
    ply: usize,
    tt_move: &Option<Move>,
) {
    // Get previous move info for countermove lookup, indexed by hashed
    // from/to squares as in the classic counter-move heuristic.
    let (prev_from_hash, prev_to_hash) = if ply > 0 {
        searcher.prev_move_stack[ply - 1]
    } else {
        (0, 0)
    };

    moves.sort_by_cached_key(|m| {
        let mut score: i32 = 0;

        // Hash move bonus
        if let Some(ttm) = tt_move {
            if m.from == ttm.from && m.to == ttm.to && m.promotion == ttm.promotion {
                score += sort_hash();
            }
        }

        if let Some(target) = game.board.get_piece(&m.to.x, &m.to.y) {
            // Capture: MVV-LVA + SEE threshold + capture history.
            let victim_val = get_piece_value(target.piece_type);
            let attacker_val = get_piece_value(m.piece.piece_type);
            let mvv_lva = victim_val * 10 - attacker_val;

            // Approximate Zig's see_threshold(pos, move, -90): treat captures
            // that lose more than ~a pawn as "losing" and others as winning.
            let see_gain = static_exchange_eval(game, m);
            let is_winning = see_gain >= see_winning_threshold();

            score += mvv_lva;
            if is_winning {
                score += sort_winning_capture();
            } else {
                score += DEFAULT_SORT_LOSING_CAPTURE;
            }

            let cap_hist =
                searcher.capture_history[m.piece.piece_type as usize][target.piece_type as usize];
            score += cap_hist / 10;
        } else {
            // Quiet move: killers + countermove + history + continuation history
            if searcher.killers[ply][0].as_ref().map_or(false, |k| {
                m.from == k.from && m.to == k.to && m.promotion == k.promotion
            }) {
                score += sort_killer1();
            } else if searcher.killers[ply][1].as_ref().map_or(false, |k| {
                m.from == k.from && m.to == k.to && m.promotion == k.promotion
            }) {
                score += sort_killer2();
            } else {
                // Check if this is the countermove for the previous move
                if ply > 0 && prev_from_hash < 256 && prev_to_hash < 256 {
                    let (cm_piece, cm_to_x, cm_to_y) =
                        searcher.countermoves[prev_from_hash][prev_to_hash];
                    if cm_piece != 0
                        && cm_piece == m.piece.piece_type as u8
                        && cm_to_x == m.to.x as i16
                        && cm_to_y == m.to.y as i16
                    {
                        score += sort_countermove();
                    }
                }
                score += DEFAULT_SORT_QUIET;

                // Main history heuristic
                let idx = hash_move_dest(m);
                score += searcher.history[m.piece.piece_type as usize][idx];

                // Continuation history: [prev_piece][prev_to][cur_from][cur_to]
                // Use 1-ply, 2-ply, and 4-ply back (like Zig: plies_ago = 0, 1, 3)
                let cur_from_hash = hash_coord_32(m.from.x, m.from.y);
                let cur_to_hash = hash_coord_32(m.to.x, m.to.y);

                for &plies_ago in &[0usize, 1, 3] {
                    if ply >= plies_ago + 1 {
                        if let Some(ref prev_move) = searcher.move_history[ply - plies_ago - 1] {
                            let prev_piece =
                                searcher.moved_piece_history[ply - plies_ago - 1] as usize;
                            if prev_piece < 16 {
                                let prev_to_hash = hash_coord_32(prev_move.to.x, prev_move.to.y);
                                score += searcher.cont_history[prev_piece][prev_to_hash]
                                    [cur_from_hash][cur_to_hash];
                            }
                        }
                    }
                }
            }
        }

        // We sort by ascending key, so negate to get highest-score moves first.
        -score
    });
}

pub fn sort_moves_root(
    searcher: &Searcher,
    game: &GameState,
    moves: &mut Vec<Move>,
    tt_move: &Option<Move>,
) {
    sort_moves(searcher, game, moves, 0, tt_move);
}

pub fn sort_captures(game: &GameState, moves: &mut Vec<Move>) {
    moves.sort_by_cached_key(|m| {
        let mut score = 0;
        if let Some(target) = game.board.get_piece(&m.to.x, &m.to.y) {
            // MVV-LVA: prioritize capturing high value pieces with low value attackers
            score -= get_piece_value(target.piece_type) * 10 - get_piece_value(m.piece.piece_type);
        }
        score
    });
}

/// Hash move destination to 256-size index (for main history)
/// Uses wrapping_abs to safely handle negative coordinates in infinite chess
#[inline]
pub fn hash_move_dest(m: &Move) -> usize {
    ((m.to.x.wrapping_abs() ^ m.to.y.wrapping_abs()) & 0xFF) as usize
}

/// Hash move source to 256-size index
#[inline]
pub fn hash_move_from(m: &Move) -> usize {
    // Use wrapping for consistency with infinite coordinates
    ((m.from.x.wrapping_abs() ^ m.from.y.wrapping_abs()) & 0xFF) as usize
}

/// Hash coordinate to 32-size index (for continuation history)
/// Uses wrapping_abs to safely handle negative coordinates in infinite chess
#[inline]
pub fn hash_coord_32(x: i64, y: i64) -> usize {
    // Use wrapping operations to avoid issues with i64::MIN
    let ux = x.wrapping_abs() as u64;
    let uy = y.wrapping_abs() as u64;
    ((ux ^ uy) & 0x1F) as usize
}
