use std::fmt::Display;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Piece {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

impl Display for Piece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Piece::Pawn => f.write_str("P"),
            Piece::Rook => f.write_str("R"),
            Piece::Knight => f.write_str("N"),
            Piece::Bishop => f.write_str("B"),
            Piece::Queen => f.write_str("Q"),
            Piece::King => f.write_str("K"),
        }
    }
}
