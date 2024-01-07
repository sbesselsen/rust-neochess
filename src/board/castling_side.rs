use std::fmt::Display;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum CastlingSide {
    Queen = 0,
    King = 1,
}
impl Display for CastlingSide {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CastlingSide::Queen => f.write_str("queenside"),
            CastlingSide::King => f.write_str("kingside"),
        }
    }
}
