use crate::board::{CastlingSide, Color, Piece};

use self::constants::ZOBRIST_RANDOM;

mod constants;

pub(crate) fn zobrist_piece(piece: Piece, color: Color, index: usize) -> u64 {
    let piece_number: usize = match piece {
        Piece::Pawn => 0,
        Piece::Knight => 2,
        Piece::Bishop => 4,
        Piece::Rook => 6,
        Piece::Queen => 8,
        Piece::King => 10,
    } + match color {
        Color::Black => 0,
        Color::White => 1,
    };

    let rank_index = 7 - (index / 8);
    let file_index = index % 8;
    let piece_offset = 64 * piece_number + 8 * rank_index + file_index;

    ZOBRIST_RANDOM[piece_offset]
}

pub(crate) fn zobrist_castling(side: CastlingSide, color: Color) -> u64 {
    let offset: usize = match side {
        CastlingSide::King => 0,
        CastlingSide::Queen => 1,
    } + match color {
        Color::Black => 2,
        Color::White => 0,
    };
    ZOBRIST_RANDOM[768 + offset]
}

pub(crate) fn zobrist_en_passant(index: usize) -> u64 {
    let file_index = index % 8;
    ZOBRIST_RANDOM[772 + file_index]
}

pub(crate) fn zobrist_color(color: Color) -> u64 {
    match color {
        Color::White => zobrist_color_swap(),
        Color::Black => 0u64,
    }
}

pub(crate) fn zobrist_color_swap() -> u64 {
    ZOBRIST_RANDOM[780]
}
