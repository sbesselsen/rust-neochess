use std::{
    fs,
    io::{self, Error, ErrorKind},
    path::Path,
};

use crate::board::BoardMove;

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
            .map_while(|i| {
                let k = self.read_key_at(i);
                if k != key {
                    return None;
                }

                // TODO: read the move
                Some(OpeningBookEntry {
                    key,
                    board_move: BoardMove::new(0, 1),
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

    use super::{OpeningBook, PolyglotOpeningBook};

    fn get_polygot_book() -> PolyglotOpeningBook {
        // TODO: include a small opening book for testing?
        let book_file = env::var("OPENING_BOOK").expect("need OPENING_BOOK variable");
        let book = PolyglotOpeningBook::read(book_file);
        assert!(book.is_ok());
        book.unwrap()
    }

    #[ignore]
    #[test]
    fn book_read() {
        get_polygot_book();
    }

    #[ignore]
    #[test]
    fn book_find_setup() {
        let book = get_polygot_book();
        let entries = book.find(0x463b96181691fc9c);
        for e in &entries {
            println!("{:?}", e);
        }
        assert_eq!(entries.len(), 0);
        assert!(entries.len() > 0);
    }
}
