use crate::bitwise_helper::BitwiseHelper;
use std::fmt::{Debug, Display};

pub const COLOR_WHITE: usize = 0;
pub const COLOR_BLACK: usize = 1;
pub const SIDE_QUEEN: usize = 0;
pub const SIDE_KING: usize = 1;

const ALL_MASK: u64 = 0xFFFFFFFFFFFFFFFF;
const RANK_0_MASK: u64 = 0x00000000000000FF;
const FILE_0_MASK: u64 = 0x8080808080808080;
const DIAG_TL_MASK: u64 = 0x8040201008040201;
const DIAG_TR_MASK: u64 = 0x0102040810204080;

enum Check {
    None,
    Check,
    Checkmate,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct BitBoard {
    // Board definition
    pub pawns: [u64; 2],
    pub rooks: [u64; 2],
    pub bishops: [u64; 2],
    pub knights: [u64; 2],
    pub queens: [u64; 2],
    pub king: [u64; 2],
    pub can_castle: [[bool; 2]; 2],
    pub active_color: usize,
    pub en_passant_square: Option<u32>,
    pub halfmove_clock: u16,
    pub fullmove_number: u16,
}

#[derive(Debug)]
pub struct FenParseError {
    message: String,
}

impl From<String> for FenParseError {
    fn from(message: String) -> Self {
        FenParseError { message }
    }
}

impl From<&str> for FenParseError {
    fn from(message: &str) -> Self {
        FenParseError {
            message: String::from(message),
        }
    }
}

impl Display for FenParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("FenParseError: ")?;
        f.write_str(&self.message)
    }
}

trait ColorHelper {
    fn wb<T>(&self, white: T, black: T) -> T;
    fn opponent(&self) -> Self;
}

impl ColorHelper for usize {
    fn wb<T>(&self, white: T, black: T) -> T {
        if *self == COLOR_WHITE {
            white
        } else {
            black
        }
    }

    fn opponent(&self) -> Self {
        self.wb(COLOR_BLACK, COLOR_WHITE)
    }
}

impl BitBoard {
    pub fn new() -> BitBoard {
        BitBoard {
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

    pub fn new_setup() -> BitBoard {
        BitBoard {
            pawns: [0x000000000000FF00, 0x00FF000000000000],
            rooks: [0x0000000000000081, 0x8100000000000000],
            knights: [0x0000000000000042, 0x4200000000000000],
            bishops: [0x0000000000000024, 0x2400000000000000],
            queens: [0x0000000000000010, 0x1000000000000000],
            king: [0x0000000000000008, 0x0800000000000000],
            can_castle: [[true, true], [true, true]],
            ..BitBoard::new()
        }
    }

    pub fn try_parse_fen(fen: &str) -> Result<BitBoard, FenParseError> {
        let parts: Vec<&str> = fen.trim().split(' ').collect();
        if parts.len() != 6 {
            return Err(FenParseError::from("Some elements are missing"));
        }

        let mut board = BitBoard::new();

        // Parse the pieces.
        let ranks: Vec<&str> = parts[0].split('/').collect();
        if ranks.len() != 8 {
            return Err(FenParseError::from("Invalid number of ranks"));
        }
        for (rank_rev_index, rank_data) in ranks.iter().enumerate() {
            let mut file_index: u32 = 0;
            for char in rank_data.chars() {
                let index = 8 * (rank_rev_index as u32) + file_index;
                match char {
                    'p' => {
                        board.pawns[COLOR_BLACK].set_bit(index, true);
                    }
                    'r' => {
                        board.rooks[COLOR_BLACK].set_bit(index, true);
                    }
                    'n' => {
                        board.knights[COLOR_BLACK].set_bit(index, true);
                    }
                    'b' => {
                        board.bishops[COLOR_BLACK].set_bit(index, true);
                    }
                    'q' => {
                        board.queens[COLOR_BLACK].set_bit(index, true);
                    }
                    'k' => {
                        board.king[COLOR_BLACK].set_bit(index, true);
                    }
                    'P' => {
                        board.pawns[COLOR_WHITE].set_bit(index, true);
                    }
                    'R' => {
                        board.rooks[COLOR_WHITE].set_bit(index, true);
                    }
                    'N' => {
                        board.knights[COLOR_WHITE].set_bit(index, true);
                    }
                    'B' => {
                        board.bishops[COLOR_WHITE].set_bit(index, true);
                    }
                    'Q' => {
                        board.queens[COLOR_WHITE].set_bit(index, true);
                    }
                    'K' => {
                        board.king[COLOR_WHITE].set_bit(index, true);
                    }
                    '1'..='8' => {
                        file_index += char
                            .to_string()
                            .parse::<u32>()
                            .expect("character in '1'..'8' range should be parseable to u32")
                            - 1;
                    }
                    _ => {
                        return Err(FenParseError::from(
                            String::from("Invalid character: ") + &char.to_string(),
                        ));
                    }
                }
                file_index += 1;
                if file_index > 8 {
                    return Err(FenParseError::from(format!(
                        "Rank has too many pieces: {}",
                        (8 - rank_rev_index)
                    )));
                }
            }
        }

        board.active_color = match parts[1] {
            "w" => COLOR_WHITE,
            "b" => COLOR_BLACK,
            _ => {
                return Err(FenParseError::from(format!(
                    "Cannot determine active color; invalid character: {}",
                    parts[1]
                )));
            }
        };

        board.can_castle[COLOR_WHITE][SIDE_KING] = parts[2].contains('K');
        board.can_castle[COLOR_WHITE][SIDE_QUEEN] = parts[2].contains('Q');
        board.can_castle[COLOR_BLACK][SIDE_KING] = parts[2].contains('k');
        board.can_castle[COLOR_BLACK][SIDE_QUEEN] = parts[2].contains('q');

        board.en_passant_square = match parts[3] {
            "-" => None,
            square => Some(
                Self::try_parse_coords(square)
                    .map(|(rank, file)| Self::index_from_rank_file(rank, file))
                    .map_err(|()| FenParseError::from("Invalid en passant square"))?,
            ),
        };

        board.halfmove_clock = parts[4]
            .parse()
            .map_err(|_| FenParseError::from("Invalid halfmove clock"))?;
        board.fullmove_number = parts[5]
            .parse()
            .map_err(|_| FenParseError::from("Invalid fullmove number"))?;

        Ok(board)
    }

    pub fn next_boards(&self) -> Vec<BitBoard> {
        let mut output = vec![];

        self.push_pawn_moves(&mut output);
        self.push_rooklike_moves(&mut output);
        self.push_bishoplike_moves(&mut output);
        self.push_knight_moves(&mut output);
        self.push_king_moves(&mut output);

        output
    }

    pub fn to_fen(&self) -> String {
        let occupancy = self.occupancy_bits();
        let mut output = String::with_capacity(90);
        for rank in (1..=8).rev() {
            let occupancy = (occupancy >> (8 * (rank - 1))) as u8;
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
                    output.push(self.square_occupant_to_char(Self::index_from_rank_file(
                        rank as u32,
                        offset + 1,
                    )));
                    offset += 1;
                }
            }
            if rank > 1 {
                output.push('/');
            }
        }
        output.push_str(self.active_color.wb(" w", " b"));

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

    pub fn to_readable_board(&self) -> String {
        let mut output = String::with_capacity(270);
        output.push('\n');

        for rank in (1..=8).rev() {
            output.push_str(&rank.to_string());
            for file in 1..=8 {
                output.push(' ');
                output.push(self.square_occupant_to_char(Self::index_from_rank_file(rank, file)));
            }
            output.push('\n');
        }
        output.push(' ');
        for file in 1..=8 {
            output.push(' ');
            output.push(Self::file_to_char(file));
        }
        output.push('\n');
        output.push_str(&self.to_fen());
        output
    }

    pub fn move_as_string(&self, after_move: &BitBoard) -> Option<String> {
        let opponent_color = self.active_color.opponent();
        let opponent_occupancy = self.occupancy_bits_for(opponent_color);

        let moved_pawns = self.pawns[self.active_color] & !after_move.pawns[self.active_color];
        let check_suffix = match after_move.check_state() {
            Check::None => "",
            Check::Check => "+",
            Check::Checkmate => "#",
        };
        if moved_pawns > 0 {
            let from_index = moved_pawns.leading_zeros();
            let mut to_index = (after_move.pawns[self.active_color]
                & !self.pawns[self.active_color])
                .leading_zeros();

            let mut promotion = String::with_capacity(2);

            if to_index == 64 {
                // Pawn promotion.
                let new_queen_index = (after_move.queens[self.active_color]
                    & !self.queens[self.active_color])
                    .leading_zeros();

                let new_rook_index = (after_move.rooks[self.active_color]
                    & !self.rooks[self.active_color])
                    .leading_zeros();

                let new_knight_index = (after_move.knights[self.active_color]
                    & !self.knights[self.active_color])
                    .leading_zeros();

                let new_bishop_index = (after_move.bishops[self.active_color]
                    & !self.bishops[self.active_color])
                    .leading_zeros();

                if new_queen_index < 64 {
                    to_index = new_queen_index;
                    promotion.push_str("=Q");
                } else if new_rook_index < 64 {
                    to_index = new_rook_index;
                    promotion.push_str("=R");
                } else if new_knight_index < 64 {
                    to_index = new_knight_index;
                    promotion.push_str("=N");
                } else if new_bishop_index < 64 {
                    to_index = new_bishop_index;
                    promotion.push_str("=B");
                }
            }

            let (_, from_file) = Self::rank_file_from_index(from_index);
            let (to_rank, to_file) = Self::rank_file_from_index(to_index);

            if to_file == from_file {
                // Pawn move.
                return Some(format!(
                    "{}{to_rank}{promotion}{check_suffix}",
                    Self::file_to_char(to_file),
                ));
            } else {
                // Pawn capture.
                return Some(format!(
                    "{}x{}{to_rank}{promotion}{check_suffix}",
                    Self::file_to_char(from_file),
                    Self::file_to_char(to_file),
                ));
            }
        };

        let moved_king = self.king[self.active_color] & !after_move.king[self.active_color];
        if moved_king > 0 {
            let home_rank_offset = self.active_color.wb(56, 0);
            if self.can_castle[self.active_color][SIDE_KING]
                && moved_king.bit_at_index(home_rank_offset + 4)
                && after_move.king[self.active_color].bit_at_index(home_rank_offset + 6)
            {
                // Castling kingside.
                return Some(String::from("O-O"));
            }
            if self.can_castle[self.active_color][SIDE_QUEEN]
                && moved_king.bit_at_index(home_rank_offset + 4)
                && after_move.king[self.active_color].bit_at_index(home_rank_offset + 2)
            {
                // Castling queenside.
                return Some(String::from("O-O-O"));
            }
            let to_index = after_move.king[self.active_color].leading_zeros();
            let capture_mark = if opponent_occupancy.bit_at_index(to_index) {
                "x"
            } else {
                ""
            };
            if to_index < 64 {
                let (to_rank, to_file) = Self::rank_file_from_index(to_index);
                return Some(format!(
                    "K{capture_mark}{}{to_rank}{check_suffix}",
                    Self::file_to_char(to_file)
                ));
            }
        }

        let moved_rook = self.rooks[self.active_color] & !after_move.rooks[self.active_color];
        if moved_rook > 0 {
            let to_index = (after_move.rooks[self.active_color] & !self.rooks[self.active_color])
                .leading_zeros();
            let to_mask = u64::from_bit(to_index);
            let capture_mark = if opponent_occupancy.bit_at_index(to_index) {
                "x"
            } else {
                ""
            };

            let (to_rank, to_file) = Self::rank_file_from_index(to_index);

            // Create a board which is like the board after the move, only our moved piece is still in
            // the original spot. That will allow us to calculate sightlines properly.
            let mut sightlines_board = after_move.clone();
            sightlines_board.rooks[self.active_color] |= self.rooks[self.active_color];

            let other_rooks = sightlines_board
                .rooklike_moves_masks(to_mask)
                .map(|(_, m)| m)
                .reduce(|a, b| a | b)
                .unwrap_or(0)
                & after_move.rooks[self.active_color];

            if other_rooks > 0 {
                // Two rooks could have moved here; we need to differentiate between them.
                let from_index = moved_rook.leading_zeros();
                let (from_rank, from_file) = Self::rank_file_from_index(from_index);

                let rooks_on_same_file =
                    ((other_rooks | moved_rook) & (FILE_0_MASK >> (from_file - 1))).count_ones()
                        > 1;

                if rooks_on_same_file {
                    return Some(format!(
                        "R{}{from_rank}{capture_mark}{}{to_rank}{check_suffix}",
                        Self::file_to_char(from_file),
                        Self::file_to_char(to_file),
                    ));
                } else {
                    return Some(format!(
                        "R{}{capture_mark}{}{to_rank}{check_suffix}",
                        Self::file_to_char(from_file),
                        Self::file_to_char(to_file),
                    ));
                }
            }

            return Some(format!(
                "R{capture_mark}{}{to_rank}{check_suffix}",
                Self::file_to_char(to_file),
            ));
        }

        let moved_knight = self.knights[self.active_color] & !after_move.knights[self.active_color];
        if moved_knight > 0 {
            let to_index = (after_move.knights[self.active_color]
                & !self.knights[self.active_color])
                .leading_zeros();
            let to_mask = u64::from_bit(to_index);
            let capture_mark = if opponent_occupancy.bit_at_index(to_index) {
                "x"
            } else {
                ""
            };

            let (to_rank, to_file) = Self::rank_file_from_index(to_index);

            let other_knights = after_move
                .knight_moves_masks(to_mask)
                .map(|(_, m)| m)
                .reduce(|a, b| a | b)
                .unwrap_or(0)
                & after_move.knights[self.active_color];

            if other_knights > 0 {
                // Two knights could have moved here; we need to differentiate between them.
                let from_index = moved_knight.leading_zeros();
                let (from_rank, from_file) = Self::rank_file_from_index(from_index);

                let knights_on_same_file = ((other_knights | moved_knight)
                    & (FILE_0_MASK >> (from_file - 1)))
                    .count_ones()
                    > 1;

                if knights_on_same_file {
                    return Some(format!(
                        "N{}{from_rank}{capture_mark}{}{to_rank}{check_suffix}",
                        Self::file_to_char(from_file),
                        Self::file_to_char(to_file),
                    ));
                } else {
                    return Some(format!(
                        "N{}{capture_mark}{}{to_rank}{check_suffix}",
                        Self::file_to_char(from_file),
                        Self::file_to_char(to_file),
                    ));
                }
            }

            return Some(format!(
                "N{capture_mark}{}{to_rank}{check_suffix}",
                Self::file_to_char(to_file),
            ));
        }

        let moved_bishop = self.bishops[self.active_color] & !after_move.bishops[self.active_color];
        if moved_bishop > 0 {
            let to_index = (after_move.bishops[self.active_color]
                & !self.bishops[self.active_color])
                .leading_zeros();
            let to_mask = u64::from_bit(to_index);
            let capture_mark = if opponent_occupancy.bit_at_index(to_index) {
                "x"
            } else {
                ""
            };

            let (to_rank, to_file) = Self::rank_file_from_index(to_index);

            // Create a board which is like the board after the move, only our moved piece is still in
            // the original spot. That will allow us to calculate sightlines properly.
            let mut sightlines_board = after_move.clone();
            sightlines_board.bishops[self.active_color] |= self.bishops[self.active_color];

            let other_bishops = sightlines_board
                .bishoplike_moves_masks(to_mask)
                .map(|(_, m)| m)
                .reduce(|a, b| a | b)
                .unwrap_or(0)
                & after_move.bishops[self.active_color];

            if other_bishops > 0 {
                // Two bishops could have moved here; we need to differentiate between them.
                let from_index = moved_bishop.leading_zeros();
                let (from_rank, from_file) = Self::rank_file_from_index(from_index);

                let bishops_on_same_file = ((other_bishops | moved_bishop)
                    & (FILE_0_MASK >> (from_file - 1)))
                    .count_ones()
                    > 1;

                if bishops_on_same_file {
                    return Some(format!(
                        "B{}{from_rank}{capture_mark}{}{to_rank}{check_suffix}",
                        Self::file_to_char(from_file),
                        Self::file_to_char(to_file),
                    ));
                } else {
                    return Some(format!(
                        "B{}{capture_mark}{}{to_rank}{check_suffix}",
                        Self::file_to_char(from_file),
                        Self::file_to_char(to_file),
                    ));
                }
            }

            return Some(format!(
                "B{capture_mark}{}{to_rank}{check_suffix}",
                Self::file_to_char(to_file),
            ));
        }

        let moved_queen = self.queens[self.active_color] & !after_move.queens[self.active_color];
        if moved_queen > 0 {
            let to_index = (after_move.queens[self.active_color] & !self.queens[self.active_color])
                .leading_zeros();
            let to_mask = u64::from_bit(to_index);
            let capture_mark = if opponent_occupancy.bit_at_index(to_index) {
                "x"
            } else {
                ""
            };

            let (to_rank, to_file) = Self::rank_file_from_index(to_index);

            // Create a board which is like the board after the move, only our moved piece is still in
            // the original spot. That will allow us to calculate sightlines properly.
            let mut sightlines_board = after_move.clone();
            sightlines_board.queens[self.active_color] |= self.queens[self.active_color];

            let other_queens = (sightlines_board
                .rooklike_moves_masks(to_mask)
                .map(|(_, m)| m)
                .reduce(|a, b| a | b)
                .unwrap_or(0)
                | sightlines_board
                    .bishoplike_moves_masks(to_mask)
                    .map(|(_, m)| m)
                    .reduce(|a, b| a | b)
                    .unwrap_or(0))
                & after_move.queens[self.active_color];

            if other_queens > 0 {
                // Two queens could have moved here; we need to differentiate between them.
                let from_index = moved_queen.leading_zeros();
                let (from_rank, from_file) = Self::rank_file_from_index(from_index);

                let queens_on_same_file =
                    ((other_queens | moved_queen) & (FILE_0_MASK >> (from_file - 1))).count_ones()
                        > 1;

                if queens_on_same_file {
                    return Some(format!(
                        "Q{}{from_rank}{capture_mark}{}{to_rank}{check_suffix}",
                        Self::file_to_char(from_file),
                        Self::file_to_char(to_file),
                    ));
                } else {
                    return Some(format!(
                        "Q{}{capture_mark}{}{to_rank}{check_suffix}",
                        Self::file_to_char(from_file),
                        Self::file_to_char(to_file),
                    ));
                }
            }

            return Some(format!(
                "Q{capture_mark}{}{to_rank}{check_suffix}",
                Self::file_to_char(to_file),
            ));
        }

        None
    }

    pub fn apply_mutation<F>(&self, f: F) -> BitBoard
    where
        F: FnOnce(&mut BitBoard),
    {
        let mut clone = self.clone();
        f(&mut clone);
        clone
    }

    pub fn apply_move<F>(&self, f: F) -> BitBoard
    where
        F: FnOnce(&mut BitBoard),
    {
        self.apply_mutation(|b| {
            b.en_passant_square = None;
            f(b);
            b.halfmove_clock += 1;
            if b.active_color == COLOR_WHITE {
                b.active_color = COLOR_BLACK;
            } else {
                b.active_color = COLOR_WHITE;
                b.fullmove_number += 1;
            }

            // Update castling based on whether the rook or king moved
            // We can probably get away with doing this on every move because it's just some unconditional bit math
            let white_king_moved = b.king[COLOR_WHITE] & 0x0000000000000008 == 0;
            let black_king_moved = b.king[COLOR_BLACK] & 0x0800000000000000 == 0;
            b.can_castle[COLOR_WHITE][SIDE_QUEEN] &=
                b.rooks[COLOR_WHITE] & 0x0000000000000080 > 0 && !white_king_moved;
            b.can_castle[COLOR_WHITE][SIDE_KING] &=
                b.rooks[COLOR_WHITE] & 0x0000000000000001 > 0 && !white_king_moved;
            b.can_castle[COLOR_BLACK][SIDE_QUEEN] &=
                b.rooks[COLOR_BLACK] & 0x8000000000000000 > 0 && !black_king_moved;
            b.can_castle[COLOR_BLACK][SIDE_KING] &=
                b.rooks[COLOR_BLACK] & 0x0100000000000000 > 0 && !black_king_moved;
        })
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

    fn clear_square(&mut self, color: usize, index: u32) {
        let mask = !(1u64 << (63 - index));
        self.pawns[color] &= mask;
        self.rooks[color] &= mask;
        self.knights[color] &= mask;
        self.bishops[color] &= mask;
        self.queens[color] &= mask;
        self.king[color] &= mask;
    }

    fn knight_moves_masks(&self, mask: u64) -> impl Iterator<Item = (u32, u64)> {
        let self_occupancy = self.occupancy_bits_for(self.active_color);

        let can_go_1_left_mask = !FILE_0_MASK;
        let can_go_2_left_mask = !FILE_0_MASK ^ (FILE_0_MASK >> 1);
        let can_go_1_right_mask = !(FILE_0_MASK >> 7);
        let can_go_2_right_mask = !(FILE_0_MASK >> 7) ^ (FILE_0_MASK >> 6);

        mask.as_bit_index_iter().map(move |index| {
            let knight_mask = u64::from_bit(index);

            let can_go_1_left = knight_mask & can_go_1_left_mask;
            let left_1 = (can_go_1_left << 17) | (can_go_1_left >> 15);

            let can_go_2_left = knight_mask & can_go_2_left_mask;
            let left_2 = (can_go_2_left << 10) | (can_go_2_left >> 6);

            let can_go_1_right = knight_mask & can_go_1_right_mask;
            let right_1 = (can_go_1_right << 15) | (can_go_1_right >> 17);

            let can_go_2_right = knight_mask & can_go_2_right_mask;
            let right_2 = (can_go_2_right << 6) | (can_go_2_right >> 10);

            (
                index,
                (left_1 | left_2 | right_1 | right_2) & !self_occupancy,
            )
        })
    }

    fn push_knight_moves(&self, output: &mut Vec<BitBoard>) {
        if self.knights[self.active_color] == 0 {
            return;
        }

        let opponent_color = self.active_color.opponent();
        let opponent_occupancy = self.occupancy_bits_for(opponent_color);

        for (index, to_mask) in self.knight_moves_masks(self.knights[self.active_color]) {
            for to_index in to_mask.as_bit_index_iter() {
                output.push(self.apply_move(|b| {
                    b.knights[self.active_color].move_bit(index, to_index);
                    if opponent_occupancy.bit_at_index(to_index) {
                        b.clear_square(opponent_color, to_index);
                    }
                }));
            }
        }
    }

    fn rooklike_moves_masks(&self, mask: u64) -> impl Iterator<Item = (u32, u64)> {
        let opponent_color = self.active_color.opponent();
        let opponent_occupancy = self.occupancy_bits_for(opponent_color);
        let self_occupancy = self.occupancy_bits_for(self.active_color);
        let occupancy = self_occupancy | opponent_occupancy;

        mask.as_bit_index_iter().map(move |index| {
            let (rank, _) = Self::rank_file_from_index(index);

            let rank_mask = RANK_0_MASK << (8 * (rank - 1));

            let mut up_mask = FILE_0_MASK.discarding_shl(64 - index);
            let up_mask_occupied = up_mask & occupancy;
            if up_mask_occupied > 0 {
                // If there are other pieces inside the mask of up-moves:
                // Check where the last occupied index is and move up until (and including) there.
                // That will include a capture of the piece on that square.
                // Then mask with !self_occupancy so we don't capture our own pieces.
                // The same method works for down_mask, left_mask and right_mask, although
                // reversed for down_mask and right_mask.
                up_mask &= ALL_MASK.discarding_shr(63 - up_mask_occupied.trailing_zeros())
                    & !self_occupancy;
            }

            let mut down_mask = FILE_0_MASK.discarding_shr(index + 8);
            let down_mask_occupied = down_mask & occupancy;
            if down_mask_occupied > 0 {
                down_mask &= ALL_MASK.discarding_shl(63 - down_mask_occupied.leading_zeros())
                    & !self_occupancy;
            }

            let mut left_mask = RANK_0_MASK.discarding_shift_lr((index as i32) - 64) & rank_mask;
            let left_mask_occupied = left_mask & occupancy;
            if left_mask_occupied > 0 {
                left_mask &= ALL_MASK.discarding_shr(63 - left_mask_occupied.trailing_zeros())
                    & !self_occupancy;
            }

            let mut right_mask = RANK_0_MASK.discarding_shift_lr((index as i32) - 55) & rank_mask;
            let right_mask_occupied = right_mask & occupancy;
            if right_mask_occupied > 0 {
                right_mask &= ALL_MASK.discarding_shl(63 - right_mask_occupied.leading_zeros())
                    & !self_occupancy;
            }

            (index, up_mask | down_mask | left_mask | right_mask)
        })
    }

    fn push_rooklike_moves(&self, output: &mut Vec<BitBoard>) {
        let rooklike_mask = self.rooks[self.active_color] | self.queens[self.active_color];

        if rooklike_mask == 0 {
            return;
        }

        let opponent_color = self.active_color.opponent();
        let opponent_occupancy = self.occupancy_bits_for(opponent_color);

        for (index, to_mask) in self.rooklike_moves_masks(rooklike_mask) {
            for to_index in to_mask.as_bit_index_iter() {
                output.push(self.apply_move(|b| {
                    b.queens[self.active_color].move_bit(index, to_index);
                    b.rooks[self.active_color].move_bit(index, to_index);
                    if opponent_occupancy.bit_at_index(to_index) {
                        b.clear_square(opponent_color, to_index);
                    }
                }));
            }
        }
    }

    fn bishoplike_moves_masks(&self, mask: u64) -> impl Iterator<Item = (u32, u64)> {
        let opponent_color = self.active_color.opponent();
        let opponent_occupancy = self.occupancy_bits_for(opponent_color);
        let self_occupancy = self.occupancy_bits_for(self.active_color);
        let occupancy = self_occupancy | opponent_occupancy;

        mask.as_bit_index_iter().map(move |index| {
            let (_, file) = Self::rank_file_from_index(index);

            let mut up_left_mask = DIAG_TL_MASK.discarding_shl(72 - index)
                & (ALL_MASK >> index.saturating_sub(9 * (file - 1)));
            let up_left_mask_occupied = up_left_mask & occupancy;
            if up_left_mask_occupied > 0 {
                up_left_mask &= ALL_MASK
                    .discarding_shr(63 - up_left_mask_occupied.trailing_zeros())
                    & !self_occupancy;
            }

            let mut up_right_mask = DIAG_TR_MASK.discarding_shl(63 - index)
                & (ALL_MASK >> index.saturating_sub(7 * (8 - file)));
            let up_right_mask_occupied = up_right_mask & occupancy;
            if up_right_mask_occupied > 0 {
                up_right_mask &= ALL_MASK
                    .discarding_shr(63 - up_right_mask_occupied.trailing_zeros())
                    & !self_occupancy;
            }

            let mut down_left_mask = DIAG_TR_MASK.discarding_shr(index)
                & (ALL_MASK << 63u32.saturating_sub(index + 7 * (file - 1)));
            let down_left_mask_occupied = down_left_mask & occupancy;
            if down_left_mask_occupied > 0 {
                down_left_mask &= ALL_MASK
                    .discarding_shl(63 - down_left_mask_occupied.leading_zeros())
                    & !self_occupancy;
            }

            let mut down_right_mask = DIAG_TL_MASK.discarding_shr(index + 9)
                & (ALL_MASK << 63u32.saturating_sub(index + 9 * (8 - file)));
            let down_right_mask_occupied = down_right_mask & occupancy;
            if down_right_mask_occupied > 0 {
                down_right_mask &= ALL_MASK
                    .discarding_shl(63 - down_right_mask_occupied.leading_zeros())
                    & !self_occupancy;
            }

            (
                index,
                up_left_mask | up_right_mask | down_left_mask | down_right_mask,
            )
        })
    }

    fn push_bishoplike_moves(&self, output: &mut Vec<BitBoard>) {
        let bishoplike_mask = self.bishops[self.active_color] | self.queens[self.active_color];

        if bishoplike_mask == 0 {
            return;
        }

        let opponent_color = self.active_color.opponent();
        let opponent_occupancy = self.occupancy_bits_for(opponent_color);

        for (index, to_mask) in self.bishoplike_moves_masks(bishoplike_mask) {
            for to_index in to_mask.as_bit_index_iter() {
                output.push(self.apply_move(|b| {
                    b.queens[self.active_color].move_bit(index, to_index);
                    b.bishops[self.active_color].move_bit(index, to_index);
                    if opponent_occupancy.bit_at_index(to_index) {
                        b.clear_square(opponent_color, to_index);
                    }
                }));
            }
        }
    }

    fn push_pawn_moves(&self, output: &mut Vec<BitBoard>) {
        if self.pawns[self.active_color] == 0 {
            return;
        }

        let occupancy = self.occupancy_bits();
        let opponent_color = self.active_color.opponent();
        let opponent_occupancy = self.occupancy_bits_for(opponent_color);

        let move_offset = self.active_color.wb(-8, 8);

        // Move 1 step forward.
        let move_1_mask = self.pawns[self.active_color].shift_lr(move_offset) & !occupancy;
        let promote_mask = move_1_mask & self.active_color.wb(RANK_0_MASK << 56, RANK_0_MASK);
        for to_index in (move_1_mask & !promote_mask).as_bit_index_iter() {
            // Normal move forward.
            let from_index = (to_index as i32) - move_offset;
            output.push(self.apply_move(|b| {
                b.pawns[self.active_color].move_bit(from_index as u32, to_index);
            }));
        }
        for to_index in promote_mask.as_bit_index_iter() {
            // Promote pawn by moving 1 step forward.
            let from_index = (to_index as i32) - move_offset;
            self.push_pawn_promotions(from_index as u32, to_index, output);
        }

        // Move 2 steps forward.
        let move_2_mask = move_1_mask.shift_lr(move_offset)
            & self.active_color.wb(RANK_0_MASK << 24, RANK_0_MASK << 32)
            & !occupancy;
        for to_index in move_2_mask.as_bit_index_iter() {
            let en_passant_index = (to_index as i32) - move_offset;
            let from_index = en_passant_index - move_offset;

            output.push(self.apply_move(|b| {
                b.pawns[self.active_color].move_bit(from_index as u32, to_index);
                b.en_passant_square = Some(en_passant_index as u32);
            }));
        }

        // Capture.
        let capture_left_mask =
            (self.pawns[self.active_color] & !FILE_0_MASK).shift_lr(move_offset - 1);
        let capture_right_mask =
            (self.pawns[self.active_color] & !(FILE_0_MASK >> 7)).shift_lr(move_offset + 1);
        let en_passant_mask = self.en_passant_square.map_or(0, u64::from_bit);
        for to_index in
            (capture_left_mask & (opponent_occupancy | en_passant_mask)).as_bit_index_iter()
        {
            let from_index = (to_index as i32) - (move_offset - 1);
            self.push_pawn_captures(from_index as u32, to_index, output);
        }
        for to_index in
            (capture_right_mask & (opponent_occupancy | en_passant_mask)).as_bit_index_iter()
        {
            let from_index = (to_index as i32) - (move_offset + 1);
            self.push_pawn_captures(from_index as u32, to_index, output);
        }
    }

    fn push_pawn_captures(&self, from_index: u32, to_index: u32, output: &mut Vec<BitBoard>) {
        let opponent_color = self.active_color.opponent();

        if self.en_passant_square == Some(to_index) {
            // Avoid the brick.
            output.push(self.apply_move(|b| {
                let clear_index = self.active_color.wb(to_index + 8, to_index - 8);
                b.pawns[self.active_color].move_bit(from_index, to_index);
                b.clear_square(opponent_color, clear_index);
            }));
        } else if !(8..=56).contains(&to_index) {
            // This is a capture with promotion.
            output.push(self.apply_move(|b| {
                b.pawns[self.active_color].set_bit(from_index, false);
                b.rooks[self.active_color].set_bit(to_index, true);
                b.clear_square(opponent_color, to_index);
            }));
            output.push(self.apply_move(|b| {
                b.pawns[self.active_color].set_bit(from_index, false);
                b.bishops[self.active_color].set_bit(to_index, true);
                b.clear_square(opponent_color, to_index);
            }));
            output.push(self.apply_move(|b| {
                b.pawns[self.active_color].set_bit(from_index, false);
                b.knights[self.active_color].set_bit(to_index, true);
                b.clear_square(opponent_color, to_index);
            }));
            output.push(self.apply_move(|b| {
                b.pawns[self.active_color].set_bit(from_index, false);
                b.queens[self.active_color].set_bit(to_index, true);
                b.clear_square(opponent_color, to_index);
            }));
        } else {
            // This is a normal capture.
            output.push(self.apply_move(|b| {
                b.pawns[self.active_color].move_bit(from_index, to_index);
                b.clear_square(opponent_color, to_index);
            }));
        }
    }

    fn push_pawn_promotions(&self, from_index: u32, to_index: u32, output: &mut Vec<BitBoard>) {
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

    fn king_moves_mask(&self, mask: u64) -> u64 {
        let self_occupancy = self.occupancy_bits_for(self.active_color);

        let can_go_left_mask = !FILE_0_MASK;
        let can_go_right_mask = !(FILE_0_MASK >> 7);

        let can_go_left = mask & can_go_left_mask;
        let left_mask = (can_go_left << 1) | (can_go_left << 9) | (can_go_left >> 7);

        let can_go_right = mask & can_go_right_mask;
        let right_mask = (can_go_right >> 1) | (can_go_right >> 9) | (can_go_right << 7);

        let up_mask = mask << 8;

        let down_mask = mask >> 8;

        (left_mask | right_mask | up_mask | down_mask) & !self_occupancy
    }

    fn push_king_moves(&self, output: &mut Vec<BitBoard>) {
        if self.king[self.active_color] == 0 {
            return;
        }

        let opponent_color = self.active_color.opponent();
        let opponent_occupancy = self.occupancy_bits_for(opponent_color);
        let self_occupancy = self.occupancy_bits_for(self.active_color);
        let occupancy = self_occupancy | opponent_occupancy;

        for index in self.king[self.active_color].as_bit_index_iter() {
            let mask = u64::from_bit(index);
            for to_index in self.king_moves_mask(mask).as_bit_index_iter() {
                output.push(self.apply_move(|b| {
                    b.king[self.active_color].move_bit(index, to_index);
                    if opponent_occupancy.bit_at_index(to_index) {
                        b.clear_square(opponent_color, to_index);
                    }
                }));
            }
        }

        if self.can_castle[self.active_color][SIDE_KING] {
            let must_be_open_mask = 6u64 << self.active_color.wb(0, 56);
            if must_be_open_mask & occupancy == 0 {
                let must_not_be_attacked_mask = 14u64 << self.active_color.wb(0, 56);
                if !self.mask_is_attacked(must_not_be_attacked_mask) {
                    // We can castle.
                    let king_index = self.active_color.wb(60, 4);
                    let rook_index = self.active_color.wb(63, 7);
                    output.push(self.apply_move(|b| {
                        b.king[self.active_color].move_bit(king_index, king_index + 2);
                        b.rooks[self.active_color].move_bit(rook_index, rook_index - 2);
                    }));
                }
            }
        }
        if self.can_castle[self.active_color][SIDE_QUEEN] {
            let must_be_open_mask = 112u64 << self.active_color.wb(0, 56);
            if must_be_open_mask & occupancy == 0 {
                let must_not_be_attacked_mask = 56u64 << self.active_color.wb(0, 56);
                if !self.mask_is_attacked(must_not_be_attacked_mask) {
                    // We can castle.
                    let king_index = self.active_color.wb(60, 4);
                    let rook_index = self.active_color.wb(56, 0);
                    output.push(self.apply_move(|b| {
                        b.king[self.active_color].move_bit(king_index, king_index - 2);
                        b.rooks[self.active_color].move_bit(rook_index, rook_index + 2);
                    }));
                }
            }
        }
    }

    fn mask_is_attacked(&self, mask: u64) -> bool {
        let opponent_color = self.active_color.opponent();

        // Is the mask attacked by pawns?
        let pawn_capture_mask = (mask & !FILE_0_MASK).shift_lr(self.active_color.wb(-9, 7))
            | (mask & !(FILE_0_MASK >> 7)).shift_lr(self.active_color.wb(-7, 9));
        if pawn_capture_mask & self.pawns[opponent_color] > 0 {
            // Attacked by a pawn!
            return true;
        }

        // Is the mask attacked by a king?
        if self.king_moves_mask(mask) & self.king[opponent_color] > 0 {
            // Attacked by a king!
            return true;
        }

        // Is the mask attacked by a knight?
        if self.knights[opponent_color] > 0 {
            let knight_moves_mask = self
                .knight_moves_masks(mask)
                .map(|(_index, moves_mask)| moves_mask)
                .reduce(|a, b| a | b)
                .expect("there should be at least 1 active bit in the mask");
            if knight_moves_mask & self.knights[opponent_color] > 0 {
                // Attacked by a knight!
                return true;
            }
        }

        let rooklike_attackers = self.rooks[opponent_color] | self.queens[opponent_color];
        if rooklike_attackers > 0 {
            let rooklike_moves_mask = self
                .rooklike_moves_masks(mask)
                .map(|(_index, moves_mask)| moves_mask)
                .reduce(|a, b| a | b)
                .expect("there should be at least 1 active bit in the mask");
            if rooklike_moves_mask & rooklike_attackers > 0 {
                // Attacked rooklike by a rook or a queen!
                return true;
            }
        }

        let bishoplike_attackers = self.bishops[opponent_color] | self.queens[opponent_color];
        if bishoplike_attackers > 0 {
            let bishoplike_moves_mask = self
                .bishoplike_moves_masks(mask)
                .map(|(_index, moves_mask)| moves_mask)
                .reduce(|a, b| a | b)
                .expect("there should be at least 1 active bit in the mask");
            if bishoplike_moves_mask & bishoplike_attackers > 0 {
                // Attacked bishoplike by a bishop or a queen!
                return true;
            }
        }

        false
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

    fn check_state(&self) -> Check {
        if self.king[self.active_color] > 0 && self.mask_is_attacked(self.king[self.active_color]) {
            let mut next_boards: Vec<BitBoard> = self.next_boards();
            let mut is_checkmate = true;
            for b in next_boards.iter_mut() {
                b.active_color = self.active_color;
                if !b.mask_is_attacked(b.king[self.active_color]) {
                    is_checkmate = false;
                    break;
                }
            }
            if is_checkmate {
                Check::Checkmate
            } else {
                Check::Check
            }
        } else {
            Check::None
        }
    }

    fn square_occupant_to_char(&self, index: u32) -> char {
        if self.pawns[COLOR_BLACK].bit_at_index(index) {
            'p'
        } else if self.pawns[COLOR_WHITE].bit_at_index(index) {
            'P'
        } else if self.rooks[COLOR_BLACK].bit_at_index(index) {
            'r'
        } else if self.rooks[COLOR_WHITE].bit_at_index(index) {
            'R'
        } else if self.knights[COLOR_BLACK].bit_at_index(index) {
            'n'
        } else if self.knights[COLOR_WHITE].bit_at_index(index) {
            'N'
        } else if self.bishops[COLOR_BLACK].bit_at_index(index) {
            'b'
        } else if self.bishops[COLOR_WHITE].bit_at_index(index) {
            'B'
        } else if self.queens[COLOR_BLACK].bit_at_index(index) {
            'q'
        } else if self.queens[COLOR_WHITE].bit_at_index(index) {
            'Q'
        } else if self.king[COLOR_BLACK].bit_at_index(index) {
            'k'
        } else if self.king[COLOR_WHITE].bit_at_index(index) {
            'K'
        } else {
            ' '
        }
    }

    fn coords_to_string(index: u32) -> String {
        let file_index = index % 8;
        let rank_index = 8 - (index / 8);
        debug_assert!(rank_index < 8, "invalid rank_index");
        Self::file_to_char(file_index + 1).to_string() + &rank_index.to_string()
    }

    fn try_parse_coords(coords: &str) -> Result<(u32, u32), ()> {
        if coords.len() != 2 {
            return Err(());
        }
        let files = " abcdefgh";
        let file = files.find(coords.chars().nth(0).unwrap()).unwrap() as u32;
        let rank = coords[1..].parse::<u32>().map_err(|_| ())?;

        Ok((rank, file))
    }

    fn file_to_char(file: u32) -> char {
        debug_assert!(file > 0 && file <= 8, "invalid file");
        let files = " abcdefgh";
        files.chars().nth(file as usize).unwrap()
    }
}

impl Default for BitBoard {
    fn default() -> Self {
        Self::new()
    }
}

impl Debug for BitBoard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BitBoard")
            .field(
                "pawns",
                &[
                    format_args!("0x{:0>16x} /* white */", self.pawns[0]),
                    format_args!("0x{:0>16x} /* black */", self.pawns[1]),
                ],
            )
            .field(
                "rooks",
                &[
                    format_args!("0x{:0>16x}", self.rooks[0]),
                    format_args!("0x{:0>16x}", self.rooks[1]),
                ],
            )
            .field(
                "bishops",
                &[
                    format_args!("0x{:0>16x}", self.bishops[0]),
                    format_args!("0x{:0>16x}", self.bishops[1]),
                ],
            )
            .field(
                "knights",
                &[
                    format_args!("0x{:0>16x}", self.knights[0]),
                    format_args!("0x{:0>16x}", self.knights[1]),
                ],
            )
            .field(
                "queens",
                &[
                    format_args!("0x{:0>16x}", self.queens[0]),
                    format_args!("0x{:0>16x}", self.queens[1]),
                ],
            )
            .field(
                "king",
                &[
                    format_args!("0x{:0>16x}", self.king[0]),
                    format_args!("0x{:0>16x}", self.king[1]),
                ],
            )
            .field(
                "can_castle",
                &[
                    &[
                        format_args!("{} /* queenside */", self.can_castle[0][0]),
                        format_args!("{} /* kingside */", self.can_castle[0][1]),
                    ],
                    &[
                        format_args!("{}", self.can_castle[1][0]),
                        format_args!("{}", self.can_castle[1][1]),
                    ],
                ],
            )
            .field(
                "active_color",
                &format_args!("{}", &self.active_color.wb("COLOR_WHITE", "COLOR_BLACK")),
            )
            .field("en_passant_square", &self.en_passant_square)
            .field("halfmove_clock", &self.halfmove_clock)
            .field("fullmove_number", &self.fullmove_number)
            .finish()
    }
}

impl Display for BitBoard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_readable_board())
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::VecDeque, time::Instant};

    use crate::{
        bitboard::{BitBoard, COLOR_BLACK, COLOR_WHITE, RANK_0_MASK},
        bitwise_helper::BitwiseHelper,
    };

    #[test]
    fn empty_board_works() {
        let board = BitBoard::new();
        assert_eq!(board.active_color, COLOR_WHITE);
        assert_eq!(board.next_boards().len(), 0);
        assert_eq!(board.occupancy_bits(), 0);
    }

    #[test]
    fn setup_board_works() {
        let normal_board = BitBoard::new_setup();
        assert_eq!(normal_board.active_color, COLOR_WHITE);
        assert_eq!(normal_board.next_boards().len(), 20);
    }

    #[test]
    fn queen_moves_correctly() {
        let mut board = BitBoard::new();
        board.queens[COLOR_WHITE] = 0x0000000008000000;
        board.rooks[COLOR_BLACK] = 0x0008000000000000;
        assert_eq!(board.next_boards().len(), 26);
    }

    #[test]
    fn king_moves_correctly() {
        let mut board = BitBoard::new_setup();
        board.king[COLOR_WHITE] |= 0x0000804200000000;

        let mut moves: Vec<BitBoard> = vec![];
        board.push_king_moves(&mut moves);

        assert_eq!(moves.len(), 19);
    }

    #[test]
    fn knight_moves_correctly() {
        let mut board = BitBoard::new_setup();
        board.knights[COLOR_WHITE] = 0x0000008000020000;

        let mut moves: Vec<BitBoard> = vec![];
        board.push_knight_moves(&mut moves);

        assert_eq!(moves.len(), 7);
    }

    #[test]
    fn rook_moves_correctly() {
        let mut board = BitBoard::new_setup();
        board.rooks[COLOR_WHITE] = 0x0000008200000000;

        let mut moves: Vec<BitBoard> = vec![];
        board.push_rooklike_moves(&mut moves);

        assert_eq!(moves.len(), 19);
    }

    #[test]
    fn pawn_moves_correctly() {
        let mut board = BitBoard::new_setup();
        board.knights[COLOR_BLACK] = 0x0000000000040000;

        let mut moves: Vec<BitBoard> = vec![];
        board.push_pawn_moves(&mut moves);

        assert_eq!(moves.len(), 16);
    }

    #[test]
    fn bishop_moves_correctly() {
        let mut board = BitBoard::new_setup();
        board.bishops[COLOR_WHITE] = 0x0000008200000000;

        let mut moves: Vec<BitBoard> = vec![];
        board.push_bishoplike_moves(&mut moves);

        assert_eq!(moves.len(), 10);
    }

    #[test]
    fn googled_en_passant() {
        let mut board = BitBoard::new_setup();
        board.pawns[COLOR_BLACK] |= 0x0000000040000000;

        let next_boards = board.next_boards();

        let en_passants: Vec<&BitBoard> = next_boards
            .iter()
            .filter(|b| b.en_passant_square.is_some())
            .collect();

        let en_passant_captures: Vec<BitBoard> = en_passants
            .iter()
            .flat_map(|b| b.next_boards())
            .filter(|b| (b.pawns[COLOR_WHITE] & (RANK_0_MASK << 24)) == 0)
            .collect();

        assert_eq!(en_passant_captures.len(), 2);
    }

    #[test]
    fn board_parser_works() {
        let fen = "r1bqkb1r/pppppppp/2n5/8/8/2N4N/PPPPPPP1/R1BQ1K1R w kq - 10 6";
        let board = BitBoard::try_parse_fen(fen);
        assert!(board.is_ok());
        assert_eq!(board.unwrap().to_fen(), fen);
    }

    fn test_move_counts(
        fen: &str,
        pawn_moves: usize,
        rooklike_moves: usize,
        knight_moves: usize,
        bishoplike_moves: usize,
        king_moves: usize,
    ) {
        let board = BitBoard::try_parse_fen(fen);
        assert!(board.is_ok());

        let board = board.unwrap();

        let mut moves: Vec<BitBoard> = vec![];
        board.push_pawn_moves(&mut moves);
        assert_eq!(moves.len(), pawn_moves);

        let mut moves: Vec<BitBoard> = vec![];
        board.push_king_moves(&mut moves);
        // Note that the king is allowed to put itself in check in next_boards().
        assert_eq!(moves.len(), king_moves);

        let mut moves: Vec<BitBoard> = vec![];
        board.push_rooklike_moves(&mut moves);
        assert_eq!(moves.len(), rooklike_moves);

        let mut moves: Vec<BitBoard> = vec![];
        board.push_bishoplike_moves(&mut moves);
        assert_eq!(moves.len(), bishoplike_moves);

        let mut moves: Vec<BitBoard> = vec![];
        board.push_knight_moves(&mut moves);
        assert_eq!(moves.len(), knight_moves);

        assert_eq!(
            board.next_boards().len(),
            pawn_moves + rooklike_moves + knight_moves + bishoplike_moves + king_moves
        );
    }

    #[test]
    fn generated_position_1_moves_correctly() {
        test_move_counts(
            "2n4K/1PR4p/2P1k3/8/2N1Pppp/8/3p1BP1/n7 w - - 0 1",
            10,
            6,
            8,
            9,
            3,
        );
    }

    #[test]
    fn generated_position_2_moves_correctly() {
        test_move_counts(
            "1R6/2p4n/2k3pr/R6p/P7/3P2PN/nK3Pp1/8 b - - 0 1",
            6,
            0,
            6,
            0,
            7,
        );
    }

    #[test]
    fn castling_for_white_works() {
        let board = BitBoard::try_parse_fen(
            "r1bqkb1r/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4",
        )
        .unwrap();
        let mut moves: Vec<BitBoard> = vec![];
        board.push_king_moves(&mut moves);

        assert_eq!(moves.len(), 3);

        // King can't castle because of a bishop.
        let board = BitBoard::try_parse_fen(
            "r2qkb1r/ppp2ppp/2np1n2/1B2p1N1/2b1P3/2N3P1/PPPP1P1P/R1BQK2R w KQkq - 1 7",
        )
        .unwrap();
        let mut moves: Vec<BitBoard> = vec![];
        board.push_king_moves(&mut moves);

        assert_eq!(moves.len(), 2);

        // King can castle on both sides.
        let board = BitBoard::try_parse_fen(
            "r1b1kb1r/1ppq1ppp/p1np4/1B2p1B1/4P1n1/2NP1N2/PPPQ1PPP/R3K2R w KQkq - 0 8",
        )
        .unwrap();
        let mut moves: Vec<BitBoard> = vec![];
        board.push_king_moves(&mut moves);

        assert_eq!(moves.len(), 5);

        // King can castle kingside but not queenside.
        let board = BitBoard::try_parse_fen(
            "r1b1kb1r/1ppq1ppp/p1Bp4/4p1B1/4P3/2NP1N2/PPPQ1nPP/R3K2R w KQkq - 0 9",
        )
        .unwrap();
        let mut moves: Vec<BitBoard> = vec![];
        board.push_king_moves(&mut moves);

        assert_eq!(moves.len(), 5);
    }

    #[test]
    fn castling_for_black_works() {
        let board = BitBoard::try_parse_fen(
            "rnbqk2r/pppp1ppp/5n2/1B2p3/1b2P3/P4N2/1PPP1PPP/RNBQK2R b KQkq - 0 4",
        )
        .unwrap();
        let mut moves: Vec<BitBoard> = vec![];
        board.push_king_moves(&mut moves);

        assert_eq!(moves.len(), 3);

        // King can't castle because it has already moved.
        let board = BitBoard::try_parse_fen(
            "rnbqk2r/pppp1ppp/5n2/1B2p3/1b2P3/P4N2/1PPP1PPP/RNBQK2R b KQq - 0 4",
        )
        .unwrap();
        let mut moves: Vec<BitBoard> = vec![];
        board.push_king_moves(&mut moves);

        assert_eq!(moves.len(), 2);

        // King can't castle queenside because of a pawn.
        let board = BitBoard::try_parse_fen(
            "r3k2r/p1Pq1ppp/1pn5/4p3/1Pb1P1n1/5N2/1PP2PPP/RNBQK2R b KQkq - 0 10",
        )
        .unwrap();
        let mut moves: Vec<BitBoard> = vec![];
        board.push_king_moves(&mut moves);

        // Again, this seems wrong, but we can put the king in check.
        assert_eq!(moves.len(), 4);
    }

    #[test]
    fn pawn_move_notation() {
        let board =
            BitBoard::try_parse_fen("2b1K3/B2P1pk1/2r3nn/1P2P2B/5pPq/4R3/3p1R2/8 w - - 0 1")
                .unwrap();

        let board2 = board.apply_move(|b| {
            b.pawns[COLOR_WHITE].move_bit(25, 17);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("b6")));

        let board2 = board.apply_move(|b| {
            b.pawns[COLOR_WHITE].move_bit(25, 18);
            b.rooks[COLOR_BLACK].set_bit(18, false);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("bxc6")));

        let board =
            BitBoard::try_parse_fen("2b1K3/B2P2k1/2r2pnn/1P2P2B/5pPq/4R3/3p1R2/8 b - - 0 1")
                .unwrap();

        let board2 = board.apply_move(|b| {
            b.pawns[COLOR_BLACK].move_bit(37, 44);
            b.rooks[COLOR_WHITE].set_bit(44, false);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("fxe3")));

        let board =
            BitBoard::try_parse_fen("2b1K3/B2P1pk1/2r3nn/1P2P2B/5pPq/4R3/3p1R2/8 w - - 0 1")
                .unwrap();

        let board2 = board.apply_move(|b| {
            b.pawns[COLOR_WHITE].set_bit(11, false);
            b.queens[COLOR_WHITE].set_bit(2, true);
            b.bishops[COLOR_BLACK].set_bit(2, false);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("dxc8=Q")));

        let board =
            BitBoard::try_parse_fen("rnbqkbnr/pppppppp/2P5/8/8/8/PP1PPPPP/RNBQKBNR w KQkq - 0 1")
                .unwrap();

        let board2 = board.apply_move(|b| {
            b.pawns[COLOR_WHITE].move_bit(18, 11);
            b.pawns[COLOR_BLACK].set_bit(11, false);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("cxd7+")));

        let board =
            BitBoard::try_parse_fen("r2bkbnr/pppppppp/2P5/8/8/4P3/PP2PPPP/RNBQKBNR w KQkq - 0 1")
                .unwrap();

        let board2 = board.apply_move(|b| {
            b.pawns[COLOR_WHITE].move_bit(18, 11);
            b.pawns[COLOR_BLACK].set_bit(11, false);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("cxd7#")));
    }

    #[test]
    fn king_move_notation() {
        let board = BitBoard::try_parse_fen(
            "r3k2r/1pp1bppp/p1np1n2/4p1q1/2BP2b1/1PN1PN1P/PBP1QPP1/R3K2R w KQkq - 0 1",
        )
        .unwrap();

        let board2 = board.apply_move(|b| {
            b.rooks[COLOR_WHITE].move_bit(63, 61);
            b.king[COLOR_WHITE].move_bit(60, 62);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("O-O")));

        let board2 = board.apply_move(|b| {
            b.rooks[COLOR_WHITE].move_bit(56, 59);
            b.king[COLOR_WHITE].move_bit(60, 58);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("O-O-O")));

        let board2 = board.apply_move(|b| {
            b.king[COLOR_WHITE].move_bit(60, 61);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("Kf1")));

        let mut board = board.clone();
        board.active_color = COLOR_BLACK;

        let board2 = board.apply_move(|b| {
            b.king[COLOR_BLACK].move_bit(4, 6);
            b.rooks[COLOR_BLACK].move_bit(7, 5);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("O-O")));

        let board2 = board.apply_move(|b| {
            b.king[COLOR_BLACK].move_bit(4, 2);
            b.rooks[COLOR_BLACK].move_bit(0, 3);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("O-O-O")));

        let board =
            BitBoard::try_parse_fen("6K1/q7/P3k2P/N1r1P2P/b2R1p2/3p3N/b1b1B1p1/8 b - - 0 1")
                .unwrap();
        let board2 = board.apply_move(|b| {
            b.king[COLOR_BLACK].move_bit(20, 28);
            b.pawns[COLOR_WHITE].set_bit(28, false);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("Kxe5+")));
    }

    #[test]
    fn rook_move_notation() {
        let board =
            BitBoard::try_parse_fen("1R3K1B/2Pp4/2p5/2r5/2P3k1/1n1P2P1/p6r/q6N b - - 0 1").unwrap();

        let board2 = board.apply_move(|b| {
            b.rooks[COLOR_BLACK].move_bit(26, 29);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("Rf5+")));

        let board2 = board.apply_move(|b| {
            b.rooks[COLOR_BLACK].move_bit(26, 31);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("Rch5")));

        let board =
            BitBoard::try_parse_fen("1R3K1B/2Pp3r/2p5/8/2P3k1/1n1P2P1/p6r/q6N b - - 0 1").unwrap();
        let board2 = board.apply_move(|b| {
            b.rooks[COLOR_BLACK].move_bit(15, 31);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("Rh7h5")));
    }

    #[test]
    fn knight_move_notation() {
        let board =
            BitBoard::try_parse_fen("R3R3/1k5p/5PP1/3qP3/n5K1/1P2PN1N/p1r3B1/5N2 w - - 0 1")
                .unwrap();

        let board2 = board.apply_move(|b| {
            b.knights[COLOR_WHITE].move_bit(45, 35);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("Nd4")));

        let board2 = board.apply_move(|b| {
            b.knights[COLOR_WHITE].move_bit(45, 51);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("Nf3d2")));

        let board2 = board.apply_move(|b| {
            b.knights[COLOR_WHITE].move_bit(45, 62);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("Nfg1")));
    }

    #[test]
    fn bishop_move_notation() {
        let board =
            BitBoard::try_parse_fen("6K1/q7/P3k2P/N1r1P2P/b2R1p2/3p3N/b1b1B1p1/8 b - - 0 1")
                .unwrap();

        let board2 = board.apply_move(|b| {
            b.bishops[COLOR_BLACK].move_bit(48, 34);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("Bc4")));

        let board2 = board.apply_move(|b| {
            b.bishops[COLOR_BLACK].move_bit(48, 57);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("Bab1")));

        let board2 = board.apply_move(|b| {
            b.bishops[COLOR_BLACK].move_bit(48, 41);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("Ba2b3")));
    }

    #[test]
    fn queen_move_notation() {
        let board =
            BitBoard::try_parse_fen("3Nbk2/1pP5/4B2K/PN2B3/3Q3p/1q1qrP2/p7/3q4 b - - 0 1").unwrap();

        let board2 = board.apply_move(|b| {
            b.queens[COLOR_BLACK].move_bit(43, 35);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("Qxd4")));

        let board2 = board.apply_move(|b| {
            b.queens[COLOR_BLACK].move_bit(43, 50);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("Qd3c2")));

        let board2 = board.apply_move(|b| {
            b.queens[COLOR_BLACK].move_bit(41, 34);
        });
        let notation = board.move_as_string(&board2);
        assert_eq!(notation, Some(String::from("Qbc4")));
    }

    #[test]
    #[ignore]
    fn make_many_boards() {
        let start = Instant::now();

        let mut queue = VecDeque::new();
        queue.push_back(BitBoard::new_setup());

        let mut counter = 0;
        while counter < 1_000_000 {
            let board = queue.pop_front().unwrap();
            for b in board.next_boards() {
                queue.push_back(b);
                counter += 1;
            }
        }

        assert!(start.elapsed().as_millis() < 300);
    }
}
