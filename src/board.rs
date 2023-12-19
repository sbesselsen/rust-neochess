use std::fmt::Debug;

use itertools::Itertools;

pub const COLOR_WHITE: usize = 0;
pub const COLOR_BLACK: usize = 1;
pub const SIDE_QUEEN: usize = 0;
pub const SIDE_KING: usize = 1;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Board {
    // Board definition
    pawns: [u64; 2],
    rooks: [u64; 2],
    bishops: [u64; 2],
    knights: [u64; 2],
    queens: [u64; 2],
    king: [u64; 2],
    can_castle: [[bool; 2]; 2],
    active_color: usize,
    en_passant_square: Option<usize>,
    halfmove_clock: u16,
    fullmove_number: u16,
}

impl Board {
    pub fn new() -> Board {
        Board {
            pawns: [0, 0],
            rooks: [0, 0],
            bishops: [0, 0],
            knights: [0, 0],
            queens: [0, 0],
            king: [0, 0],
            can_castle: [[false, false], [false, false]],
            active_color: COLOR_WHITE,
            en_passant_square: None,
            halfmove_clock: 0,
            fullmove_number: 1,
        }
    }

    pub fn new_setup() -> Board {
        Board {
            pawns: [0x000000000000FF00, 0x00FF000000000000],
            rooks: [0x0000000000000081, 0x8100000000000000],
            knights: [0x0000000000000042, 0x4200000000000000],
            bishops: [0x0000000000000024, 0x2400000000000000],
            queens: [0x0000000000000010, 0x1000000000000000],
            king: [0x0000000000000008, 0x0800000000000000],
            can_castle: [[true, true], [true, true]],
            ..Board::new()
        }
    }

    pub fn next_boards(&self) -> Vec<Board> {
        let mut output = vec![];

        self.push_pawn_moves(&mut output);

        output
    }

    fn clone_next_move(&self) -> Board {
        self.apply_move(|_| {})
    }

    fn apply_move<F>(&self, f: F) -> Board
    where
        F: FnOnce(&mut Board),
    {
        let mut clone = self.clone();
        f(&mut clone);
        clone.halfmove_clock += 1;
        if clone.active_color == COLOR_WHITE {
            clone.active_color = COLOR_BLACK;
        } else {
            clone.active_color = COLOR_WHITE;
            clone.fullmove_number += 1;
        }
        clone
    }

    fn push_pawn_moves(&self, output: &mut Vec<Board>) {
        let pawns = self.pawns[self.active_color];
        let occupancy = self.occupancy_bits();
        let start_rank = match self.active_color {
            COLOR_WHITE => 2,
            _ => 7,
        };
        let move_offset: i32 = match self.active_color {
            COLOR_WHITE => -8,
            _ => 8,
        };

        for index in pawns.into_bit_index_iter() {
            let rank = 8 - (index / 8);
            let move_1_index: u32 = ((index as i32) + move_offset)
                .try_into()
                .expect("index should be on the board");
            if !occupancy.bit_at_index(move_1_index) {
                // Move 1 place forward.
                output.push(self.apply_move(|b| {
                    b.pawns[self.active_color].set_bit(index, false);
                    b.pawns[self.active_color].set_bit(move_1_index, true);
                }));
            }
            if rank == start_rank {
                let move_2_index: u32 = ((index as i32) + 2 * move_offset)
                    .try_into()
                    .expect("index should be on the board");
                if !occupancy.bit_at_index(move_2_index) {
                    // Move 2 places forward.
                    output.push(self.apply_move(|b| {
                        b.pawns[self.active_color].set_bit(index, false);
                        b.pawns[self.active_color].set_bit(move_2_index, true);
                    }));
                }
            }
        }
    }

    fn occupancy_bits(&self) -> u64 {
        self.occupancy_bits_for(COLOR_WHITE) | self.occupancy_bits_for(COLOR_BLACK)
    }

    fn occupancy_bits_for(&self, color: usize) -> u64 {
        self.pawns[color]
            | self.rooks[color]
            | self.knights[color]
            | self.bishops[color]
            | self.queens[color]
            | self.king[color]
    }

    pub fn to_fen(&self) -> String {
        let occupancy = self.occupancy_bits();
        let mut output = (1..=8)
            .rev()
            .map(|rank| {
                let mut output = String::with_capacity(8);

                let pawns = self.pawns.map(|p| Self::rank_byte(p, rank));
                let rooks = self.rooks.map(|p| Self::rank_byte(p, rank));
                let knights = self.knights.map(|p| Self::rank_byte(p, rank));
                let bishops = self.bishops.map(|p| Self::rank_byte(p, rank));
                let queens = self.queens.map(|p| Self::rank_byte(p, rank));
                let king = self.king.map(|p| Self::rank_byte(p, rank));
                let occupancy = Self::rank_byte(occupancy, rank);
                let mut offset: u32 = 0;
                while offset < 8 {
                    let offset_mask = 128u8 >> offset;
                    let remaining_occupancy = occupancy << offset;
                    let empty_squares = remaining_occupancy.leading_zeros();
                    if empty_squares > 0 {
                        if remaining_occupancy == 0 {
                            output.push_str(&(8 - offset).to_string());
                        } else {
                            output.push_str(&empty_squares.to_string());
                        }
                        offset += empty_squares;
                    } else {
                        if pawns[COLOR_BLACK] & offset_mask > 0 {
                            output.push('p');
                        } else if pawns[COLOR_WHITE] & offset_mask > 0 {
                            output.push('P');
                        } else if rooks[COLOR_BLACK] & offset_mask > 0 {
                            output.push('r');
                        } else if rooks[COLOR_WHITE] & offset_mask > 0 {
                            output.push('R');
                        } else if knights[COLOR_BLACK] & offset_mask > 0 {
                            output.push('n');
                        } else if knights[COLOR_WHITE] & offset_mask > 0 {
                            output.push('N');
                        } else if bishops[COLOR_BLACK] & offset_mask > 0 {
                            output.push('b');
                        } else if bishops[COLOR_WHITE] & offset_mask > 0 {
                            output.push('B');
                        } else if queens[COLOR_BLACK] & offset_mask > 0 {
                            output.push('q');
                        } else if queens[COLOR_WHITE] & offset_mask > 0 {
                            output.push('Q');
                        } else if king[COLOR_BLACK] & offset_mask > 0 {
                            output.push('k');
                        } else if king[COLOR_WHITE] & offset_mask > 0 {
                            output.push('K');
                        }
                        offset += 1;
                    }
                }

                output
            })
            .join("/");
        output.push_str(if self.active_color == COLOR_WHITE {
            " w"
        } else {
            " b"
        });

        let mut can_castle = false;
        output.push(' ');
        if self.can_castle[COLOR_WHITE][SIDE_KING] {
            output.push('K');
            can_castle = true;
        }
        if self.can_castle[COLOR_WHITE][SIDE_QUEEN] {
            output.push('Q');
            can_castle = true;
        }
        if self.can_castle[COLOR_BLACK][SIDE_KING] {
            output.push('k');
            can_castle = true;
        }
        if self.can_castle[COLOR_BLACK][SIDE_QUEEN] {
            output.push('q');
            can_castle = true;
        }
        if !can_castle {
            output.push('-');
        }
        output.push(' ');
        if let Some(square) = self.en_passant_square {
            output.push_str(&Self::coords_to_string(square));
        } else {
            output.push('-');
        }
        output.push(' ');
        output.push_str(&self.halfmove_clock.to_string());
        output.push(' ');
        output.push_str(&self.fullmove_number.to_string());

        output
    }

    fn rank_byte(bits: u64, rank: usize) -> u8 {
        debug_assert!(rank > 0, "rank == 0");
        debug_assert!(rank <= 8, "rank > 8");
        (bits >> 8 * (rank - 1)) as u8
    }

    fn coords_to_string(index: usize) -> String {
        let file = index % 8;
        let rank = 8 - (index / 8);
        let files = "abcdefgh";
        files.chars().nth(file).unwrap().to_string() + &rank.to_string()
    }
}

impl Default for Board {
    fn default() -> Self {
        Self::new()
    }
}

impl Debug for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_fen())
    }
}

struct BitIndexIterator {
    n: u64,
}

impl Iterator for BitIndexIterator {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.n == 0 {
            None
        } else {
            let result = self.n.leading_zeros();
            // Flip the first bit
            self.n = self.n ^ (1u64 << (63 - result));
            Some(result)
        }
    }
}

trait BitwiseHelper {
    fn into_bit_index_iter(&self) -> BitIndexIterator;
    fn bit_at_index(&self, index: u32) -> bool;
    fn set_bit(&mut self, index: u32, value: bool);
    fn with_bit(&self, index: u32, value: bool) -> Self;
}

impl BitwiseHelper for u64 {
    fn into_bit_index_iter(&self) -> BitIndexIterator {
        BitIndexIterator { n: *self }
    }
    fn bit_at_index(&self, index: u32) -> bool {
        *self & (1u64 << (63 - index)) > 0
    }
    fn set_bit(&mut self, index: u32, value: bool) {
        *self = if value {
            *self | (1u64 << (63 - index))
        } else {
            *self & (u64::MAX ^ (1u64 << (63 - index)))
        }
    }
    fn with_bit(&self, index: u32, value: bool) -> Self {
        let mut result = self.clone();
        result.set_bit(index, value);
        result
    }
}
