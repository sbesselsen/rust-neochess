use crate::bitwise_helper::BitwiseHelper;
use std::fmt::Debug;

use itertools::Itertools;

pub const COLOR_WHITE: usize = 0;
pub const COLOR_BLACK: usize = 1;
pub const SIDE_QUEEN: usize = 0;
pub const SIDE_KING: usize = 1;

const KNIGHT_MOVES: &'static [(i32, i32)] = &[
    (2, -1),
    (2, 1),
    (1, -2),
    (1, 2),
    (-1, -2),
    (-1, 2),
    (-2, -1),
    (-2, 1),
];

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
    en_passant_square: Option<u32>,
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

    pub fn new_test() -> Board {
        let mut board = Board::new();
        board.bishops[COLOR_WHITE] = 0x0000000000000040;
        board.active_color = COLOR_WHITE;
        board
    }

    pub fn next_boards(&self) -> Vec<Board> {
        let mut output = vec![];

        self.push_pawn_moves(&mut output);
        self.push_rook_moves(&mut output);
        self.push_bishop_moves(&mut output);
        self.push_knight_moves(&mut output);

        output
    }

    fn apply_move<F>(&self, f: F) -> Board
    where
        F: FnOnce(&mut Board),
    {
        let mut clone = self.clone();
        clone.en_passant_square = None;
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

    fn rank_file_from_index(index: u32) -> (u32, u32) {
        debug_assert!(index < 64, "invalid square index");
        (8 - (index / 8), (index % 8 + 1))
    }

    fn index_from_rank_file(rank: u32, file: u32) -> u32 {
        debug_assert!(rank > 0 && rank <= 8, "invalid rank");
        debug_assert!(file > 0 && file <= 8, "invalid file");
        (8 - rank) * 8 + file - 1
    }

    fn opponent_color(color: usize) -> usize {
        if color == COLOR_WHITE {
            COLOR_BLACK
        } else {
            COLOR_WHITE
        }
    }

    fn clear_square(&mut self, color: usize, index: u32) {
        self.pawns[color].set_bit(index, false);
        self.rooks[color].set_bit(index, false);
        self.knights[color].set_bit(index, false);
        self.bishops[color].set_bit(index, false);
        self.queens[color].set_bit(index, false);
        self.king[color].set_bit(index, false);
    }

    fn push_knight_moves(&self, output: &mut Vec<Board>) {
        let opponent_color = Self::opponent_color(self.active_color);
        let opponent_occupancy = self.occupancy_bits_for(opponent_color);
        let occupancy = self.occupancy_bits_for(self.active_color) | opponent_occupancy;

        for index in self.knights[self.active_color].into_bit_index_iter() {
            let (rank, file) = Self::rank_file_from_index(index);
            for (rank_offset, file_offset) in KNIGHT_MOVES {
                let new_rank = (rank as i32) + rank_offset;
                let new_file = (file as i32) + file_offset;
                if new_rank > 0 && new_rank <= 8 && new_file > 0 && new_file <= 8 {
                    let new_index = Self::index_from_rank_file(new_rank as u32, new_file as u32);
                    if opponent_occupancy.bit_at_index(new_index) {
                        // Capture.
                        output.push(self.apply_move(|b| {
                            b.knights[self.active_color].set_bit(index, false);
                            b.knights[self.active_color].set_bit(new_index, true);
                            b.clear_square(opponent_color, new_index);
                        }));
                    } else if !occupancy.bit_at_index(new_index) {
                        // Move.
                        output.push(self.apply_move(|b| {
                            b.knights[self.active_color].set_bit(index, false);
                            b.knights[self.active_color].set_bit(new_index, true);
                        }));
                    }
                }
            }
        }
    }

    fn push_rook_moves(&self, output: &mut Vec<Board>) {
        let opponent_color = Self::opponent_color(self.active_color);
        let opponent_occupancy = self.occupancy_bits_for(opponent_color);
        let occupancy = self.occupancy_bits_for(self.active_color) | opponent_occupancy;

        let mut rook_move = |index: u32, new_index: u32| -> bool {
            if opponent_occupancy.bit_at_index(new_index) {
                // Capture and then stop.
                output.push(self.apply_move(|b| {
                    b.rooks[self.active_color].set_bit(index, false);
                    b.rooks[self.active_color].set_bit(new_index, true);
                    b.clear_square(opponent_color, new_index);
                }));
                false
            } else if occupancy.bit_at_index(new_index) {
                // Occupied by my own piece; stop.
                false
            } else {
                // Move.
                output.push(self.apply_move(|b| {
                    b.rooks[self.active_color].set_bit(index, false);
                    b.rooks[self.active_color].set_bit(new_index, true);
                }));
                true // Further moves may exist in this direction
            }
        };

        for index in self.rooks[self.active_color].into_bit_index_iter() {
            let (rank, file) = Self::rank_file_from_index(index);
            // Down
            for rank_offset in 1..rank {
                if !rook_move(index, index + 8 * rank_offset) {
                    break;
                }
            }
            // Up
            for rank_offset in 1..=(8 - rank) {
                if !rook_move(index, index - 8 * rank_offset) {
                    break;
                }
            }
            // Left
            for file_offset in 1..file {
                if !rook_move(index, index - file_offset) {
                    break;
                }
            }
            // Right
            for file_offset in 1..=(8 - file) {
                if !rook_move(index, index + file_offset) {
                    break;
                }
            }
        }
    }

    fn push_bishop_moves(&self, output: &mut Vec<Board>) {
        let opponent_color = Self::opponent_color(self.active_color);
        let opponent_occupancy = self.occupancy_bits_for(opponent_color);
        let occupancy = self.occupancy_bits_for(self.active_color) | opponent_occupancy;

        let mut bishop_move = |index: u32, new_index: u32| -> bool {
            if opponent_occupancy.bit_at_index(new_index) {
                // Capture and then stop.
                output.push(self.apply_move(|b| {
                    b.bishops[self.active_color].set_bit(index, false);
                    b.bishops[self.active_color].set_bit(new_index, true);
                    b.clear_square(opponent_color, new_index);
                }));
                false
            } else if occupancy.bit_at_index(new_index) {
                // Occupied by my own piece; stop.
                false
            } else {
                // Move.
                output.push(self.apply_move(|b| {
                    b.bishops[self.active_color].set_bit(index, false);
                    b.bishops[self.active_color].set_bit(new_index, true);
                }));
                true // Further moves may exist in this direction
            }
        };

        for index in self.bishops[self.active_color].into_bit_index_iter() {
            let (rank, file) = Self::rank_file_from_index(index);
            // Down right
            for offset in 1..(rank.min(9 - file)) {
                if !bishop_move(index, index + 9 * offset) {
                    break;
                }
            }
            // Down left
            for offset in 1..(rank.min(file)) {
                if !bishop_move(index, index + 7 * offset) {
                    break;
                }
            }
            // Up right
            for offset in 1..((9 - rank).min(9 - file)) {
                if !bishop_move(index, index - 7 * offset) {
                    break;
                }
            }
            // Up left
            for offset in 1..((9 - rank).min(file)) {
                if !bishop_move(index, index - 9 * offset) {
                    break;
                }
            }
        }
    }

    fn push_pawn_moves(&self, output: &mut Vec<Board>) {
        let occupancy = self.occupancy_bits();
        let start_rank = match self.active_color {
            COLOR_WHITE => 2,
            _ => 7,
        };
        let move_offset: i32 = match self.active_color {
            COLOR_WHITE => -8,
            _ => 8,
        };

        for index in self.pawns[self.active_color].into_bit_index_iter() {
            let (rank, file) = Self::rank_file_from_index(index);

            let move_1_index: u32 = ((index as i32) + move_offset)
                .try_into()
                .expect("index should be on the board");
            if !occupancy.bit_at_index(move_1_index) {
                // Move 1 place forward.
                if (self.active_color == COLOR_WHITE && rank == 7)
                    || (self.active_color == COLOR_BLACK && rank == 2)
                {
                    // Promote the pawn!
                    self.push_pawn_promotions(index, move_1_index, output);
                } else {
                    // Just move forward.
                    output.push(self.apply_move(|b| {
                        b.pawns[self.active_color].set_bit(index, false);
                        b.pawns[self.active_color].set_bit(move_1_index, true);
                    }));
                }
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
                        b.en_passant_square = Some(move_1_index);
                    }));
                }
            }
            let opponent_occupancy =
                self.occupancy_bits_for(Self::opponent_color(self.active_color));
            if file > 1 {
                let capture_left_index = move_1_index - 1;
                if opponent_occupancy.bit_at_index(capture_left_index) {
                    self.push_pawn_captures(index, capture_left_index, output);
                }
                if self.en_passant_square == Some(capture_left_index) {
                    self.push_pawn_captures(index, capture_left_index, output);
                }
            }
            if file < 8 {
                let capture_right_index = move_1_index - 1;
                if opponent_occupancy.bit_at_index(capture_right_index) {
                    self.push_pawn_captures(index, capture_right_index, output);
                }
                if self.en_passant_square == Some(capture_right_index) {
                    self.push_pawn_captures(index, capture_right_index, output);
                }
            }
        }
    }

    fn push_pawn_captures(&self, from_index: u32, to_index: u32, output: &mut Vec<Board>) {
        let opponent_color = Self::opponent_color(self.active_color);

        let (to_rank, _) = Self::rank_file_from_index(to_index);

        if to_rank == 1 || to_rank == 8 {
            // This is a capture with promotion.
            output.push(self.apply_move(|b| {
                b.pawns[self.active_color].set_bit(from_index, false);
                b.rooks[self.active_color].set_bit(to_index, true);
                Self::clear_square(b, opponent_color, to_index);
            }));
            output.push(self.apply_move(|b| {
                b.pawns[self.active_color].set_bit(from_index, false);
                b.bishops[self.active_color].set_bit(to_index, true);
                Self::clear_square(b, opponent_color, to_index);
            }));
            output.push(self.apply_move(|b| {
                b.pawns[self.active_color].set_bit(from_index, false);
                b.knights[self.active_color].set_bit(to_index, true);
                Self::clear_square(b, opponent_color, to_index);
            }));
            output.push(self.apply_move(|b| {
                b.pawns[self.active_color].set_bit(from_index, false);
                b.queens[self.active_color].set_bit(to_index, true);
                Self::clear_square(b, opponent_color, to_index);
            }));
        } else {
            // This is a normal capture.
            output.push(self.apply_move(|b| {
                b.pawns[self.active_color].set_bit(from_index, false);
                b.pawns[self.active_color].set_bit(to_index, true);
                Self::clear_square(b, opponent_color, to_index);
            }));
        }
    }

    fn push_pawn_promotions(&self, from_index: u32, to_index: u32, output: &mut Vec<Board>) {
        output.push(self.apply_move(|b| {
            b.pawns[self.active_color].set_bit(from_index, false);
            b.rooks[self.active_color].set_bit(to_index, true);
        }));
        output.push(self.apply_move(|b| {
            b.pawns[self.active_color].set_bit(from_index, false);
            b.bishops[self.active_color].set_bit(to_index, true);
        }));
        output.push(self.apply_move(|b| {
            b.pawns[self.active_color].set_bit(from_index, false);
            b.knights[self.active_color].set_bit(to_index, true);
        }));
        output.push(self.apply_move(|b| {
            b.pawns[self.active_color].set_bit(from_index, false);
            b.queens[self.active_color].set_bit(to_index, true);
        }));
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

                let occupancy = (occupancy >> 8 * (rank - 1)) as u8;
                let mut offset: u32 = 0;
                while offset < 8 {
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
                        let index = Self::index_from_rank_file(rank as u32, offset + 1);

                        if self.pawns[COLOR_BLACK].bit_at_index(index) {
                            output.push('p');
                        } else if self.pawns[COLOR_WHITE].bit_at_index(index) {
                            output.push('P');
                        } else if self.rooks[COLOR_BLACK].bit_at_index(index) {
                            output.push('r');
                        } else if self.rooks[COLOR_WHITE].bit_at_index(index) {
                            output.push('R');
                        } else if self.knights[COLOR_BLACK].bit_at_index(index) {
                            output.push('n');
                        } else if self.knights[COLOR_WHITE].bit_at_index(index) {
                            output.push('N');
                        } else if self.bishops[COLOR_BLACK].bit_at_index(index) {
                            output.push('b');
                        } else if self.bishops[COLOR_WHITE].bit_at_index(index) {
                            output.push('B');
                        } else if self.queens[COLOR_BLACK].bit_at_index(index) {
                            output.push('q');
                        } else if self.queens[COLOR_WHITE].bit_at_index(index) {
                            output.push('Q');
                        } else if self.king[COLOR_BLACK].bit_at_index(index) {
                            output.push('k');
                        } else if self.king[COLOR_WHITE].bit_at_index(index) {
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

    fn coords_to_string(index: u32) -> String {
        let file_index = index % 8;
        let rank_index = 8 - (index / 8);
        debug_assert!(rank_index < 8, "invalid rank_index");
        debug_assert!(file_index < 8, "invalid file_index");
        let files = "abcdefgh";
        files.chars().nth(file_index as usize).unwrap().to_string() + &rank_index.to_string()
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
