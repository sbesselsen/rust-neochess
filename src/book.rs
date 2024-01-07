use std::{
    fs,
    io::{self, Error, ErrorKind},
    path::Path,
};

use crate::board::{BoardMove, Piece};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct OpeningBookEntry {
    key: u64,
    board_move: BoardMove,
    weight: u16,
    learn: u32,
}

pub trait OpeningBook {
    fn find(&self, key: u64) -> Vec<OpeningBookEntry>;
}

pub struct EmptyOpeningBook;

impl EmptyOpeningBook {
    pub fn new() -> EmptyOpeningBook {
        EmptyOpeningBook {}
    }
}

impl OpeningBook for EmptyOpeningBook {
    fn find(&self, key: u64) -> Vec<OpeningBookEntry> {
        Vec::with_capacity(0)
    }
}

pub struct PolyglotOpeningBook {
    data: Vec<u8>,
}

impl PolyglotOpeningBook {
    pub fn read<P: AsRef<Path>>(path: P) -> io::Result<PolyglotOpeningBook> {
        let data = fs::read(path)?;
        if data.len() % 16 != 0 {
            return Err(Error::new(
                ErrorKind::InvalidData,
                "incorrect opening book size",
            ));
        }
        Ok(PolyglotOpeningBook { data })
    }

    fn binary_search(&self, key: u64, start: usize, end: usize) -> Option<usize> {
        let mid = (end - start) / 2 + start;
        let k = self.read_key_at(mid);
        if k == key {
            Some(mid)
        } else if start >= end {
            None
        } else if k > key {
            self.binary_search(key, start, mid)
        } else if mid == start {
            self.binary_search(key, end, end)
        } else {
            self.binary_search(key, mid, end)
        }
    }

    fn read_key_at(&self, index: usize) -> u64 {
        let start = index * 16;
        let end = start + 8;
        let bytes: [u8; 8] = self.data[start..end]
            .try_into()
            .expect("index should be valid");
        u64::from_be_bytes(bytes)
    }

    fn read_move_at(&self, index: usize) -> Option<BoardMove> {
        let start = index * 16 + 8;
        let end = start + 2;
        let bytes: [u8; 2] = self.data[start..end]
            .try_into()
            .expect("index should be valid");
        let move_encoded = u16::from_be_bytes(bytes);
        if move_encoded == 0 {
            // Apparently there could be null moves in here.
            return None;
        }
        let to_file = move_encoded & 0x7;
        let to_rank = (move_encoded >> 3) & 0x7;
        let to_index = ((7 - to_rank) * 8 + to_file) as u32;
        let from_file = (move_encoded >> 6) & 0x7;
        let from_rank = (move_encoded >> 9) & 0x7;
        let from_index = ((7 - from_rank) * 8 + from_file) as u32;
        let promotion_piece = match (move_encoded >> 12) & 0x7 {
            1 => Some(Piece::Knight),
            2 => Some(Piece::Bishop),
            3 => Some(Piece::Rook),
            4 => Some(Piece::Queen),
            _ => None,
        };
        if let Some(promote_to) = promotion_piece {
            Some(BoardMove::new_promotion(from_index, to_index, promote_to))
        } else {
            Some(BoardMove::new(from_index, to_index))
        }
    }

    fn read_weight_at(&self, index: usize) -> u16 {
        let start = index * 16 + 10;
        let end = start + 2;
        let bytes: [u8; 2] = self.data[start..end]
            .try_into()
            .expect("index should be valid");
        u16::from_be_bytes(bytes)
    }

    fn read_learn_at(&self, index: usize) -> u32 {
        let start = index * 16 + 12;
        let end = start + 4;
        let bytes: [u8; 4] = self.data[start..end]
            .try_into()
            .expect("index should be valid");
        u32::from_be_bytes(bytes)
    }

    fn read_entries_around(&self, index: usize) -> Vec<OpeningBookEntry> {
        let key = self.read_key_at(index);

        let start_index = (0..index)
            .rev()
            .take_while(|&i| self.read_key_at(i) == key)
            .last()
            .expect("at least one index should have the right key");

        (start_index..self.last_index())
            .take_while(|&i| self.read_key_at(i) == key)
            .filter_map(|i| {
                self.read_move_at(i).map(|board_move| OpeningBookEntry {
                    key,
                    board_move,
                    weight: self.read_weight_at(i),
                    learn: self.read_learn_at(i),
                })
            })
            .collect()
    }

    fn last_index(&self) -> usize {
        self.data.len() / 16 - 1
    }
}

impl OpeningBook for PolyglotOpeningBook {
    fn find(&self, key: u64) -> Vec<OpeningBookEntry> {
        self.binary_search(key, 0, self.last_index())
            .map(|index| self.read_entries_around(index))
            .unwrap_or_else(|| vec![])
    }
}

#[cfg(test)]
mod tests {
    use std::env;

    use super::{EmptyOpeningBook, OpeningBook, PolyglotOpeningBook};

    fn get_polygot_book() -> PolyglotOpeningBook {
        // TODO: include a small opening book for testing?
        let book_file = env::var("OPENING_BOOK").expect("need OPENING_BOOK variable");
        let book = PolyglotOpeningBook::read(book_file);
        assert!(book.is_ok());
        book.unwrap()
    }

    #[test_with::env(OPENING_BOOK)]
    #[test]
    fn book_read() {
        get_polygot_book();
    }

    #[test_with::env(OPENING_BOOK)]
    #[test]
    fn book_find_setup() {
        let book = get_polygot_book();
        let entries = book.find(0x463b96181691fc9c);
        assert!(entries.len() > 0);
        // First entry should be King's pawn
        assert_eq!(entries[0].board_move.from_index, 52);
        assert_eq!(entries[0].board_move.to_index, 36);

        // Queen's pawn ought to be in there too.
        assert!(entries
            .iter()
            .any(|e| e.board_move.from_index == 51 && e.board_move.to_index == 35));
    }

    #[test]
    fn empty_book() {
        let book = EmptyOpeningBook::new();
        assert_eq!(book.find(0x463b96181691fc9c).len(), 0);
    }
}
