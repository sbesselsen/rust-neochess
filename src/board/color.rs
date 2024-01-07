use std::fmt::Display;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Color {
    White = 0,
    Black = 1,
}
impl Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Color::White => f.write_str("w"),
            Color::Black => f.write_str("b"),
        }
    }
}
