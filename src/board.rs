use crate::bitwise_helper::BitwiseHelper;
use std::fmt::Debug;

pub const COLOR_WHITE: usize = 0;
pub const COLOR_BLACK: usize = 1;
pub const SIDE_QUEEN: usize = 0;
pub const SIDE_KING: usize = 1;

pub const ALL_MASK: u64 = 0xFFFFFFFFFFFFFFFF;
pub const RANK_0_MASK: u64 = 0x00000000000000FF;
pub const FILE_0_MASK: u64 = 0x8080808080808080;

const KNIGHT_MOVES: &[(i32, i32)] = &[
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

impl Debug for FenParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("FenParseError: ")?;
        f.write_str(&self.message)
    }
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

    pub fn try_parse_fen(fen: &str) -> Result<Board, FenParseError> {
        let parts: Vec<&str> = fen.trim().split(' ').collect();
        if parts.len() != 6 {
            return Err(FenParseError::from("Some elements are missing"));
        }

        let mut board = Board::new();

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

    pub fn next_boards(&self) -> Vec<Board> {
        let mut output = vec![];

        self.push_pawn_moves(&mut output);
        self.push_rooklike_moves(&mut output);
        self.push_bishoplike_moves(&mut output);
        self.push_knight_moves(&mut output);
        self.push_king_moves(&mut output);

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

        // Update castling based on whether the rook or king moved
        // We can probably get away with doing this on every move because it's just some unconditional bit math
        let white_king_moved = clone.king[COLOR_WHITE] & 0x0000000000000008 == 0;
        let black_king_moved = clone.king[COLOR_BLACK] & 0x0800000000000000 == 0;
        clone.can_castle[COLOR_WHITE][SIDE_QUEEN] &=
            clone.rooks[COLOR_WHITE] & 0x0000000000000080 > 0 && !white_king_moved;
        clone.can_castle[COLOR_WHITE][SIDE_KING] &=
            clone.rooks[COLOR_WHITE] & 0x0000000000000001 > 0 && !white_king_moved;
        clone.can_castle[COLOR_BLACK][SIDE_QUEEN] &=
            clone.rooks[COLOR_BLACK] & 0x8000000000000000 > 0 && !black_king_moved;
        clone.can_castle[COLOR_BLACK][SIDE_KING] &=
            clone.rooks[COLOR_BLACK] & 0x0100000000000000 > 0 && !black_king_moved;

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
        if self.knights[self.active_color] == 0 {
            return;
        }

        let opponent_color = Self::opponent_color(self.active_color);
        let opponent_occupancy = self.occupancy_bits_for(opponent_color);
        let occupancy = self.occupancy_bits_for(self.active_color) | opponent_occupancy;

        for index in self.knights[self.active_color].as_bit_index_iter() {
            let (rank, file) = Self::rank_file_from_index(index);
            for (rank_offset, file_offset) in KNIGHT_MOVES {
                let new_rank = (rank as i32) + rank_offset;
                let new_file = (file as i32) + file_offset;
                if new_rank > 0 && new_rank <= 8 && new_file > 0 && new_file <= 8 {
                    let new_index = Self::index_from_rank_file(new_rank as u32, new_file as u32);
                    if opponent_occupancy.bit_at_index(new_index) {
                        // Capture.
                        output.push(self.apply_move(|b| {
                            b.knights[self.active_color].move_bit(index, new_index);
                            b.clear_square(opponent_color, new_index);
                        }));
                    } else if !occupancy.bit_at_index(new_index) {
                        // Move.
                        output.push(self.apply_move(|b| {
                            b.knights[self.active_color].move_bit(index, new_index);
                        }));
                    }
                }
            }
        }
    }

    fn push_rooklike_moves(&self, output: &mut Vec<Board>) {
        let rooklike_mask = self.rooks[self.active_color] | self.queens[self.active_color];

        if rooklike_mask == 0 {
            return;
        }

        let opponent_color = Self::opponent_color(self.active_color);
        let opponent_occupancy = self.occupancy_bits_for(opponent_color);
        let occupancy = self.occupancy_bits_for(self.active_color) | opponent_occupancy;

        let mut rook_move = |index: u32, new_index: u32, is_queen: bool| -> bool {
            if opponent_occupancy.bit_at_index(new_index) {
                // Capture and then stop.
                output.push(self.apply_move(|b| {
                    if is_queen {
                        b.queens[self.active_color].move_bit(index, new_index);
                    } else {
                        b.rooks[self.active_color].move_bit(index, new_index);
                    }
                    b.clear_square(opponent_color, new_index);
                }));
                false
            } else if occupancy.bit_at_index(new_index) {
                // Occupied by my own piece; stop.
                false
            } else {
                // Move.
                output.push(self.apply_move(|b| {
                    if is_queen {
                        b.queens[self.active_color].move_bit(index, new_index);
                    } else {
                        b.rooks[self.active_color].move_bit(index, new_index);
                    }
                }));
                true // Further moves may exist in this direction
            }
        };

        for index in rooklike_mask.as_bit_index_iter() {
            let (rank, file) = Self::rank_file_from_index(index);
            let is_queen = self.queens[self.active_color].bit_at_index(index);

            // Down
            for rank_offset in 1..rank {
                if !rook_move(index, index + 8 * rank_offset, is_queen) {
                    break;
                }
            }
            // Up
            for rank_offset in 1..=(8 - rank) {
                if !rook_move(index, index - 8 * rank_offset, is_queen) {
                    break;
                }
            }
            // Left
            for file_offset in 1..file {
                if !rook_move(index, index - file_offset, is_queen) {
                    break;
                }
            }
            // Right
            for file_offset in 1..=(8 - file) {
                if !rook_move(index, index + file_offset, is_queen) {
                    break;
                }
            }
        }
    }

    fn push_bishoplike_moves(&self, output: &mut Vec<Board>) {
        let bishoplike_mask = self.bishops[self.active_color] | self.queens[self.active_color];

        if bishoplike_mask == 0 {
            return;
        }

        let opponent_color = Self::opponent_color(self.active_color);
        let opponent_occupancy = self.occupancy_bits_for(opponent_color);
        let occupancy = self.occupancy_bits_for(self.active_color) | opponent_occupancy;

        let mut bishop_move = |index: u32, new_index: u32, is_queen: bool| -> bool {
            if opponent_occupancy.bit_at_index(new_index) {
                // Capture and then stop.
                output.push(self.apply_move(|b| {
                    if is_queen {
                        b.queens[self.active_color].move_bit(index, new_index);
                    } else {
                        b.bishops[self.active_color].move_bit(index, new_index);
                    }
                    b.clear_square(opponent_color, new_index);
                }));
                false
            } else if occupancy.bit_at_index(new_index) {
                // Occupied by my own piece; stop.
                false
            } else {
                // Move.
                output.push(self.apply_move(|b| {
                    if is_queen {
                        b.queens[self.active_color].move_bit(index, new_index);
                    } else {
                        b.bishops[self.active_color].move_bit(index, new_index);
                    }
                }));
                true // Further moves may exist in this direction
            }
        };

        for index in bishoplike_mask.as_bit_index_iter() {
            let (rank, file) = Self::rank_file_from_index(index);
            let is_queen = self.queens[self.active_color].bit_at_index(index);

            // Down right
            for offset in 1..(rank.min(9 - file)) {
                if !bishop_move(index, index + 9 * offset, is_queen) {
                    break;
                }
            }
            // Down left
            for offset in 1..(rank.min(file)) {
                if !bishop_move(index, index + 7 * offset, is_queen) {
                    break;
                }
            }
            // Up right
            for offset in 1..((9 - rank).min(9 - file)) {
                if !bishop_move(index, index - 7 * offset, is_queen) {
                    break;
                }
            }
            // Up left
            for offset in 1..((9 - rank).min(file)) {
                if !bishop_move(index, index - 9 * offset, is_queen) {
                    break;
                }
            }
        }
    }

    fn push_pawn_moves(&self, output: &mut Vec<Board>) {
        if self.pawns[self.active_color] == 0 {
            return;
        }

        let occupancy = self.occupancy_bits();
        let start_rank = match self.active_color {
            COLOR_WHITE => 2,
            _ => 7,
        };
        let move_offset: i32 = match self.active_color {
            COLOR_WHITE => -8,
            _ => 8,
        };

        for index in self.pawns[self.active_color].as_bit_index_iter() {
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
                        b.pawns[self.active_color].move_bit(index, move_1_index);
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
                        b.pawns[self.active_color].move_bit(index, move_2_index);
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
                let capture_right_index = move_1_index + 1;
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
                b.pawns[self.active_color].move_bit(from_index, to_index);
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

    fn push_king_moves(&self, output: &mut Vec<Board>) {
        if self.king[self.active_color] == 0 {
            return;
        }

        let opponent_color = Self::opponent_color(self.active_color);
        let opponent_occupancy = self.occupancy_bits_for(opponent_color);
        let occupancy = self.occupancy_bits_for(self.active_color) | opponent_occupancy;

        for index in self.king[self.active_color].as_bit_index_iter() {
            let (rank, file) = Self::rank_file_from_index(index);
            for rank_offset in -1..=1 {
                for file_offset in -1..=1 {
                    if rank_offset == 0 && file_offset == 0 {
                        continue;
                    }
                    let new_rank = (rank as i32) + rank_offset;
                    let new_file = (file as i32) + file_offset;
                    if new_rank > 0 && new_rank <= 8 && new_file > 0 && new_file <= 8 {
                        let new_index =
                            Self::index_from_rank_file(new_rank as u32, new_file as u32);
                        if opponent_occupancy.bit_at_index(new_index) {
                            // Capture.
                            output.push(self.apply_move(|b| {
                                b.king[self.active_color].move_bit(index, new_index);
                                b.clear_square(opponent_color, new_index);
                            }));
                        } else if !occupancy.bit_at_index(new_index) {
                            // Move.
                            output.push(self.apply_move(|b| {
                                b.king[self.active_color].move_bit(index, new_index);
                            }));
                        }
                    }
                }
            }
        }

        // This implementation is highly optimized to work only for castling, don't use it anywhere else.
        // For example: we assume that squares_mask does not contain any squares on A or H file,
        // because the king doesn't pass those when castling.
        let any_squares_attacked = |squares_mask: u64| {
            // Attacks by pawns
            let unshifted_pawns_mask = (squares_mask << 1) | (squares_mask >> 1);
            let pawns_mask = if self.active_color == COLOR_WHITE {
                unshifted_pawns_mask << 8
            } else {
                unshifted_pawns_mask >> 8
            };
            if pawns_mask & self.pawns[opponent_color] > 0 {
                // A pawn attacks some of the squares
                return true;
            }

            // Attacks by king
            let unshifted_king_mask = squares_mask | (squares_mask << 1) | (squares_mask >> 1);
            let king_mask = if self.active_color == COLOR_WHITE {
                unshifted_king_mask << 8
            } else {
                unshifted_king_mask >> 8
            };
            if king_mask & self.king[opponent_color] > 0 {
                // The opponent's king attacks some of the squares.
                return true;
            }

            let king_rank = if self.active_color == COLOR_WHITE {
                8
            } else {
                1
            };
            let unshifted_squares_mask = squares_mask << ((king_rank - 1) * 8);
            let files_mask = unshifted_squares_mask
                | (unshifted_squares_mask >> 8)
                | (unshifted_squares_mask >> 16)
                | (unshifted_squares_mask >> 24)
                | (unshifted_squares_mask >> 32)
                | (unshifted_squares_mask >> 48)
                | (unshifted_squares_mask >> 56)
                | (unshifted_squares_mask >> 8)
                | (unshifted_squares_mask >> 16)
                | (unshifted_squares_mask >> 24)
                | (unshifted_squares_mask >> 32)
                | (unshifted_squares_mask >> 48)
                | (unshifted_squares_mask >> 56);

            // Rooklike attacks (rook and queen).
            // We only check vertical attacks because horizontal checks are ruled out by this point.
            let rooklike_attackers =
                (self.rooks[opponent_color] | self.queens[opponent_color]) & files_mask;
            if rooklike_attackers > 0 {
                // Some rooks are potentially on the right files.
                for attacker_index in rooklike_attackers.as_bit_index_iter() {
                    let (attacker_rank, attacker_file) = Self::rank_file_from_index(attacker_index);
                    let attacker_file_mask = FILE_0_MASK >> (attacker_file - 1);

                    // Find other pieces on this file (excluding the attackers and the king).
                    let file_occupancy =
                        (occupancy ^ rooklike_attackers ^ squares_mask) & attacker_file_mask;

                    if file_occupancy == 0 {
                        // There is nothing else on this file; the attacker can get to the square.
                        return true;
                    }

                    let min_rank = attacker_rank.min(king_rank);
                    let max_rank = attacker_rank.max(king_rank);
                    let between_mask =
                        (ALL_MASK << (8 * min_rank)) & (ALL_MASK >> (8 * (9 - max_rank)));
                    if file_occupancy & between_mask == 0 {
                        // There are no pieces between the attacker and the target square.
                        return true;
                    }
                }
            }

            // Knight attacks.
            // We can only do this because we know the squares_mask does not extend to A or H file.
            let king_rank_mask = RANK_0_MASK << (8 * (king_rank - 1));
            let unshifted_knight_spots_1_lr = (squares_mask << 1) | (squares_mask >> 1);
            let unshifted_knight_spots_2_lr =
                ((squares_mask << 2) | squares_mask >> 2) & king_rank_mask;
            let knight_spots = if self.active_color == COLOR_WHITE {
                (unshifted_knight_spots_1_lr << 16) | (unshifted_knight_spots_2_lr << 8)
            } else {
                (unshifted_knight_spots_1_lr >> 16) | (unshifted_knight_spots_2_lr >> 8)
            };
            if knight_spots & self.knights[opponent_color] > 0 {
                // We have a knight who can attack some of our squares.
                return true;
            }

            // Bishoplike attacks.
            // These may come from parallellograms going left up and right up from the squares_mask.
            let potential_bishoplike_squares = if self.active_color == COLOR_WHITE {
                (((squares_mask << 7) | (squares_mask << 9)) & (RANK_0_MASK << 8))
                    | (((squares_mask << 14) | (squares_mask << 18)) & (RANK_0_MASK << 16))
                    | (((squares_mask << 21) | (squares_mask << 27)) & (RANK_0_MASK << 24))
                    | (((squares_mask << 28) | (squares_mask << 36)) & (RANK_0_MASK << 32))
                    | (((squares_mask << 35) | (squares_mask << 45)) & (RANK_0_MASK << 40))
                    | (((squares_mask << 42) | (squares_mask << 54)) & (RANK_0_MASK << 48))
                    | (((squares_mask << 49) | (squares_mask << 63)) & (RANK_0_MASK << 56))
            } else {
                (((squares_mask >> 7) | (squares_mask >> 9)) & (RANK_0_MASK << 48))
                    | (((squares_mask >> 14) | (squares_mask >> 18)) & (RANK_0_MASK << 40))
                    | (((squares_mask >> 21) | (squares_mask >> 27)) & (RANK_0_MASK << 32))
                    | (((squares_mask >> 28) | (squares_mask >> 36)) & (RANK_0_MASK << 24))
                    | (((squares_mask >> 35) | (squares_mask >> 45)) & (RANK_0_MASK << 16))
                    | (((squares_mask >> 42) | (squares_mask >> 54)) & (RANK_0_MASK << 8))
                    | (((squares_mask >> 49) | (squares_mask >> 63)) & RANK_0_MASK)
            };
            let bishoplike_attackers = (self.bishops[opponent_color] | self.queens[opponent_color])
                & potential_bishoplike_squares;
            if bishoplike_attackers > 0 {
                println!("Bishoplike attacks not implemented yet!");
                // There are bishops around (unchecked whether they are in attacking position).
                // TODO!
            }

            false
        };

        let rank_bit_offset = if self.active_color == COLOR_WHITE {
            0
        } else {
            7 * 8
        };

        if self.can_castle[self.active_color][SIDE_KING] {
            let travel_squares = 6u64 << rank_bit_offset;
            if occupancy & travel_squares == 0 {
                // The squares between the king and the rook are empty.
                let king_travel_squares = self.king[self.active_color] | travel_squares;
                if !any_squares_attacked(king_travel_squares) {
                    // We can castle!
                    output.push(self.apply_move(|b| {
                        b.king[self.active_color] = 2u64 << rank_bit_offset;
                        b.rooks[self.active_color] ^= 5u64 << rank_bit_offset;
                    }));
                }
            }
        }
        if self.can_castle[self.active_color][SIDE_QUEEN] {
            let travel_squares = 112u64 << rank_bit_offset;
            if occupancy & travel_squares == 0 {
                // The squares between the king and the rook are empty.
                let king_travel_squares = self.king[self.active_color] | (48u64 << rank_bit_offset);
                if !any_squares_attacked(king_travel_squares) {
                    // We can castle!
                    output.push(self.apply_move(|b| {
                        b.king[self.active_color] = 32u64 << rank_bit_offset;
                        b.rooks[self.active_color] ^= 144u64 << rank_bit_offset;
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

impl Default for Board {
    fn default() -> Self {
        Self::new()
    }
}

impl Debug for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_readable_board())
    }
}

#[cfg(test)]
mod tests {
    use crate::board::{Board, COLOR_BLACK, COLOR_WHITE};

    #[test]
    fn empty_board_works() {
        let board = Board::new();
        assert_eq!(board.active_color, COLOR_WHITE);
        assert_eq!(board.next_boards().len(), 0);
        assert_eq!(board.occupancy_bits(), 0);
    }
    #[test]
    fn setup_board_works() {
        let normal_board = Board::new_setup();
        assert_eq!(normal_board.active_color, COLOR_WHITE);
        assert_eq!(normal_board.next_boards().len(), 20);
    }
    #[test]
    fn queen_moves_correctly() {
        let mut board = Board::new();
        board.queens[COLOR_WHITE] = 0x0000000008000000;
        board.rooks[COLOR_BLACK] = 0x0008000000000000;
        assert_eq!(board.next_boards().len(), 26);
    }

    #[test]
    fn board_parser_works() {
        let fen = "r1bqkb1r/pppppppp/2n5/8/8/2N4N/PPPPPPP1/R1BQ1K1R w kq - 10 6";
        let board = Board::try_parse_fen(fen);
        assert!(board.is_ok());
        assert!(board.unwrap().to_fen() == fen);
    }
}
