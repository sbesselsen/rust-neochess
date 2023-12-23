use crate::bitwise_helper::BitwiseHelper;
use std::fmt::Debug;

pub const COLOR_WHITE: usize = 0;
pub const COLOR_BLACK: usize = 1;
pub const SIDE_QUEEN: usize = 0;
pub const SIDE_KING: usize = 1;

pub const ALL_MASK: u64 = 0xFFFFFFFFFFFFFFFF;
pub const RANK_0_MASK: u64 = 0x00000000000000FF;
pub const FILE_0_MASK: u64 = 0x8080808080808080;
pub const DIAG_TL_MASK: u64 = 0x8040201008040201;
pub const DIAG_TR_MASK: u64 = 0x0102040810204080;

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

trait ColorDecide {
    fn wb<T>(&self, white: T, black: T) -> T;
}

impl ColorDecide for usize {
    fn wb<T>(&self, white: T, black: T) -> T {
        if *self == COLOR_WHITE {
            white
        } else {
            black
        }
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
        color.wb(COLOR_BLACK, COLOR_WHITE)
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
        let active_color_occupancy = self.occupancy_bits_for(self.active_color);

        let can_go_1_left_mask = !FILE_0_MASK;
        let can_go_2_left_mask = !FILE_0_MASK ^ (FILE_0_MASK >> 1);
        let can_go_1_right_mask = !(FILE_0_MASK >> 7);
        let can_go_2_right_mask = !(FILE_0_MASK >> 7) ^ (FILE_0_MASK >> 6);

        for index in self.knights[self.active_color].as_bit_index_iter() {
            let knight_mask = u64::from_bit(index);

            let can_go_1_left = knight_mask & can_go_1_left_mask;
            let left_1 = (can_go_1_left << 17) | (can_go_1_left >> 15);

            let can_go_2_left = knight_mask & can_go_2_left_mask;
            let left_2 = (can_go_2_left << 10) | (can_go_2_left >> 6);

            let can_go_1_right = knight_mask & can_go_1_right_mask;
            let right_1 = (can_go_1_right << 15) | (can_go_1_right >> 17);

            let can_go_2_right = knight_mask & can_go_2_right_mask;
            let right_2 = (can_go_2_right << 6) | (can_go_2_right >> 10);

            let moved_mask = (left_1 | left_2 | right_1 | right_2) & !active_color_occupancy;

            for to_index in moved_mask.as_bit_index_iter() {
                output.push(self.apply_move(|b| {
                    b.knights[self.active_color].move_bit(index, to_index);
                    if opponent_occupancy.bit_at_index(to_index) {
                        b.clear_square(opponent_color, to_index);
                    }
                }));
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
        let self_occupancy = self.occupancy_bits_for(self.active_color);
        let occupancy = self_occupancy | opponent_occupancy;

        for index in rooklike_mask.as_bit_index_iter() {
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

            let to_mask = up_mask | down_mask | left_mask | right_mask;
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

    fn push_bishoplike_moves(&self, output: &mut Vec<Board>) {
        let bishoplike_mask = self.bishops[self.active_color] | self.queens[self.active_color];

        if bishoplike_mask == 0 {
            return;
        }

        let opponent_color = Self::opponent_color(self.active_color);
        let opponent_occupancy = self.occupancy_bits_for(opponent_color);
        let self_occupancy = self.occupancy_bits_for(self.active_color);
        let occupancy = self_occupancy | opponent_occupancy;

        for index in bishoplike_mask.as_bit_index_iter() {
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

            let to_mask = up_left_mask | up_right_mask | down_left_mask | down_right_mask;
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

    fn push_pawn_moves(&self, output: &mut Vec<Board>) {
        if self.pawns[self.active_color] == 0 {
            return;
        }

        let occupancy = self.occupancy_bits();
        let opponent_color = Self::opponent_color(self.active_color);
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

    fn push_pawn_captures(&self, from_index: u32, to_index: u32, output: &mut Vec<Board>) {
        let opponent_color = Self::opponent_color(self.active_color);

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
        let self_occupancy = self.occupancy_bits_for(self.active_color);

        let can_go_left_mask = !FILE_0_MASK;
        let can_go_right_mask = !(FILE_0_MASK >> 7);

        for index in self.king[self.active_color].as_bit_index_iter() {
            let mask = u64::from_bit(index);

            let can_go_left = mask & can_go_left_mask;
            let left_mask = (can_go_left << 1) | (can_go_left << 9) | (can_go_left >> 7);

            let can_go_right = mask & can_go_right_mask;
            let right_mask = (can_go_right >> 1) | (can_go_right >> 9) | (can_go_right << 7);

            let up_mask = mask << 8;

            let down_mask = mask >> 8;

            let moves_mask = (left_mask | right_mask | up_mask | down_mask) & !self_occupancy;

            for to_index in moves_mask.as_bit_index_iter() {
                output.push(self.apply_move(|b| {
                    b.king[self.active_color].move_bit(index, to_index);
                    if opponent_occupancy.bit_at_index(to_index) {
                        b.clear_square(opponent_color, to_index);
                    }
                }));
            }
        }

        // TODO: castling
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
    use crate::board::{Board, COLOR_BLACK, COLOR_WHITE, RANK_0_MASK};

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
    fn king_moves_correctly() {
        let mut board = Board::new_setup();
        board.king[COLOR_WHITE] |= 0x0000804200000000;

        let mut moves: Vec<Board> = vec![];
        board.push_king_moves(&mut moves);

        assert_eq!(moves.len(), 19);
    }

    #[test]
    fn knight_moves_correctly() {
        let mut board = Board::new_setup();
        board.knights[COLOR_WHITE] = 0x0000008000020000;

        let mut moves: Vec<Board> = vec![];
        board.push_knight_moves(&mut moves);

        assert_eq!(moves.len(), 7);
    }

    #[test]
    fn rook_moves_correctly() {
        let mut board = Board::new_setup();
        board.rooks[COLOR_WHITE] = 0x0000008200000000;

        let mut moves: Vec<Board> = vec![];
        board.push_rooklike_moves(&mut moves);

        assert_eq!(moves.len(), 19);
    }

    #[test]
    fn pawn_moves_correctly() {
        let mut board = Board::new_setup();
        board.knights[COLOR_BLACK] = 0x0000000000040000;

        let mut moves: Vec<Board> = vec![];
        board.push_pawn_moves(&mut moves);

        assert_eq!(moves.len(), 16);
    }

    #[test]
    fn bishop_moves_correctly() {
        let mut board = Board::new_setup();
        board.bishops[COLOR_WHITE] = 0x0000008200000000;

        let mut moves: Vec<Board> = vec![];
        board.push_bishoplike_moves(&mut moves);

        assert_eq!(moves.len(), 10);
    }

    #[test]
    fn googled_en_passant() {
        let mut board = Board::new_setup();
        board.pawns[COLOR_BLACK] |= 0x0000000040000000;

        let next_boards = board.next_boards();

        let en_passants: Vec<&Board> = next_boards
            .iter()
            .filter(|b| b.en_passant_square.is_some())
            .collect();

        let en_passant_captures: Vec<Board> = en_passants
            .iter()
            .flat_map(|b| b.next_boards())
            .filter(|b| (b.pawns[COLOR_WHITE] & (RANK_0_MASK << 24)) == 0)
            .collect();

        assert_eq!(en_passant_captures.len(), 2);
    }

    #[test]
    fn board_parser_works() {
        let fen = "r1bqkb1r/pppppppp/2n5/8/8/2N4N/PPPPPPP1/R1BQ1K1R w kq - 10 6";
        let board = Board::try_parse_fen(fen);
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
        let board = Board::try_parse_fen(fen);
        assert!(board.is_ok());

        let board = board.unwrap();

        let mut moves: Vec<Board> = vec![];
        board.push_pawn_moves(&mut moves);
        assert_eq!(moves.len(), pawn_moves);

        let mut moves: Vec<Board> = vec![];
        board.push_king_moves(&mut moves);
        // Note that the king is allowed to put itself in check in next_boards().
        assert_eq!(moves.len(), king_moves);

        let mut moves: Vec<Board> = vec![];
        board.push_rooklike_moves(&mut moves);
        assert_eq!(moves.len(), rooklike_moves);

        let mut moves: Vec<Board> = vec![];
        board.push_bishoplike_moves(&mut moves);
        assert_eq!(moves.len(), bishoplike_moves);

        let mut moves: Vec<Board> = vec![];
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
}
