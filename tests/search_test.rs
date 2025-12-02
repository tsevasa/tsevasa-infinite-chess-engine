use hydrochess_wasm::board::{Piece, PieceType, PlayerColor};
use hydrochess_wasm::game::GameState;

// Helper to setup a specific board state
fn setup_mate_in_one() -> GameState {
    let mut game = GameState::new();

    // Scenario: Back-rank mate
    // Black King at (0, 8)
    // Black Pawns at (-1, 7), (0, 7), (1, 7) blocking escape
    // White Rook at (7, 1)
    // White King at (0, 10) blocking escape from other side
    // White to move. Rook (7, 1) -> (7, 8) is mate.

    game.board
        .set_piece(0, 8, Piece::new(PieceType::King, PlayerColor::Black));
    game.board
        .set_piece(-1, 7, Piece::new(PieceType::Pawn, PlayerColor::Black));
    game.board
        .set_piece(0, 7, Piece::new(PieceType::Pawn, PlayerColor::Black));
    game.board
        .set_piece(1, 7, Piece::new(PieceType::Pawn, PlayerColor::Black));

    game.board
        .set_piece(7, 1, Piece::new(PieceType::Rook, PlayerColor::White));
    game.board
        .set_piece(0, 10, Piece::new(PieceType::King, PlayerColor::White));

    game.turn = PlayerColor::White;
    game.material_score = 200;

    game
}

fn setup_2queen_vs_king() -> GameState {
    let mut game = GameState::new();

    game.board
        .set_piece(0, 8, Piece::new(PieceType::King, PlayerColor::Black));
    game.board
        .set_piece(3, 9, Piece::new(PieceType::Queen, PlayerColor::White));
    game.board
        .set_piece(-5, 8, Piece::new(PieceType::Queen, PlayerColor::White));

    game.turn = PlayerColor::White;
    game.material_score = 1800;

    game
}

fn setup_classical_start_position() -> GameState {
    let mut game = GameState::new();

    // White back rank
    game.board
        .set_piece(1, 1, Piece::new(PieceType::Rook, PlayerColor::White));
    game.board
        .set_piece(2, 1, Piece::new(PieceType::Knight, PlayerColor::White));
    game.board
        .set_piece(3, 1, Piece::new(PieceType::Bishop, PlayerColor::White));
    game.board
        .set_piece(4, 1, Piece::new(PieceType::Queen, PlayerColor::White));
    game.board
        .set_piece(5, 1, Piece::new(PieceType::King, PlayerColor::White));
    game.board
        .set_piece(6, 1, Piece::new(PieceType::Bishop, PlayerColor::White));
    game.board
        .set_piece(7, 1, Piece::new(PieceType::Knight, PlayerColor::White));
    game.board
        .set_piece(8, 1, Piece::new(PieceType::Rook, PlayerColor::White));

    // White pawns
    for file in 1..=8 {
        game.board
            .set_piece(file, 2, Piece::new(PieceType::Pawn, PlayerColor::White));
    }

    // Black back rank
    game.board
        .set_piece(1, 8, Piece::new(PieceType::Rook, PlayerColor::Black));
    game.board
        .set_piece(2, 8, Piece::new(PieceType::Knight, PlayerColor::Black));
    game.board
        .set_piece(3, 8, Piece::new(PieceType::Bishop, PlayerColor::Black));
    game.board
        .set_piece(4, 8, Piece::new(PieceType::Queen, PlayerColor::Black));
    game.board
        .set_piece(5, 8, Piece::new(PieceType::King, PlayerColor::Black));
    game.board
        .set_piece(6, 8, Piece::new(PieceType::Bishop, PlayerColor::Black));
    game.board
        .set_piece(7, 8, Piece::new(PieceType::Knight, PlayerColor::Black));
    game.board
        .set_piece(8, 8, Piece::new(PieceType::Rook, PlayerColor::Black));

    // Black pawns
    for file in 1..=8 {
        game.board
            .set_piece(file, 7, Piece::new(PieceType::Pawn, PlayerColor::Black));
    }

    game.turn = PlayerColor::White;

    // Set material score to the sum of piece values (white minus black)
    let mut score = 0;
    for (_, piece) in &game.board.pieces {
        let val = hydrochess_wasm::evaluation::get_piece_value(piece.piece_type);
        match piece.color {
            PlayerColor::White => score += val,
            PlayerColor::Black => score -= val,
            PlayerColor::Neutral => {}
        }
    }
    game.material_score = score;

    // Ensure piece counts and spatial indices are consistent
    game.recompute_piece_counts();

    game
}

#[test]
fn test_mate_in_one() {
    let mut game = setup_mate_in_one();

    // Print all legal moves to debug
    let moves = game.get_legal_moves();
    println!("Available moves: {}", moves.len());
    for m in &moves {
        println!(
            "  Move: ({},{}) -> ({},{})",
            m.from.x, m.from.y, m.to.x, m.to.y
        );
    }

    // Search with iterative deepening (depth 10)
    let best_move = hydrochess_wasm::search::get_best_move(&mut game, 10);

    assert!(best_move.is_some(), "Should find a move");
    let m = best_move.unwrap();

    println!(
        "Best move chosen: from ({}, {}) to ({}, {})",
        m.from.x, m.from.y, m.to.x, m.to.y
    );

    // Verify the engine finds the checkmate move: Rook (7,1) -> (7,8)
    assert_eq!(m.from.x, 7, "Move should be from rook at x=7");
    assert_eq!(m.from.y, 1, "Move should be from rook at y=1");
    assert_eq!(m.to.x, 7, "Move should be to x=7");
    assert_eq!(m.to.y, 8, "Move should be to y=8 (checkmate square)");
}

#[test]
fn test_2queen() {
    let mut game = setup_2queen_vs_king();

    // Print all legal moves to debug
    let moves = game.get_legal_moves();
    println!("Available moves: {}", moves.len());
    for m in &moves {
        println!(
            "  Move: ({},{}) -> ({},{})",
            m.from.x, m.from.y, m.to.x, m.to.y
        );
    }

    // Search with iterative deepening (depth 10)
    let best_move = hydrochess_wasm::search::get_best_move(&mut game, 10);

    assert!(best_move.is_some(), "Should find a move");
    let m = best_move.unwrap();

    println!(
        "Best move chosen: from ({}, {}) to ({}, {})",
        m.from.x, m.from.y, m.to.x, m.to.y
    );
}

#[test]
fn test_no_wing_rook_opening() {
    let mut game = setup_classical_start_position();

    // Use a moderate depth to let opening heuristics and eval influence the choice
    let best_move = hydrochess_wasm::search::get_best_move(&mut game, 6);

    assert!(
        best_move.is_some(),
        "Engine should find a move from the starting position",
    );
    let m = best_move.unwrap();

    println!(
        "Opening move chosen: from ({}, {}) to ({}, {})",
        m.from.x, m.from.y, m.to.x, m.to.y
    );

    // Regression: avoid pure wing rook shuffles like R1,1->-1,1 or R8,1->9,1 as first move.
    assert!(
        !((m.from.x == 1 && m.from.y == 1 && m.to.x == -1 && m.to.y == 1)
            || (m.from.x == 8 && m.from.y == 1 && m.to.x == 9 && m.to.y == 1)),
        "Engine should not choose early wing rook shuffles R1,1->-1,1 or R8,1->9,1 as its first move",
    );
}
