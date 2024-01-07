use std::fmt::Display;

use super::{piece::Piece, Board};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct BoardMove {
    pub from_index: u32,
    pub to_index: u32,
    pub promote_to: Option<Piece>,
}

impl BoardMove {
    pub fn new(from_index: u32, to_index: u32) -> BoardMove {
        BoardMove {
            from_index,
            to_index,
            promote_to: None,
        }
    }

    pub fn new_promotion(from_index: u32, to_index: u32, promote_to: Piece) -> BoardMove {
        BoardMove {
            from_index,
            to_index,
            promote_to: Some(promote_to),
        }
    }
}

impl Display for BoardMove {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = String::with_capacity(5);
        output.push_str(&Board::coords_to_string(self.from_index));
        output.push('-');
        output.push_str(&Board::coords_to_string(self.to_index));
        f.write_str(&output)?;
        if let Some(piece) = self.promote_to {
            f.write_str("=")?;
            Display::fmt(&piece, f)?;
        }
        Ok(())
    }
}
