use crate::{
    bitwise_helper::BitwiseHelper,
    board::{Board, COLOR_BLACK, COLOR_WHITE},
    evaluator::Evaluator,
    score::Score,
};

use self::constants::{
    ENDGAME_BISHOP_VALUES, ENDGAME_KING_VALUES, ENDGAME_KNIGHT_VALUES, ENDGAME_PAWN_VALUES,
    ENDGAME_PIECE_VALUES, ENDGAME_QUEEN_VALUES, ENDGAME_ROOK_VALUES, GAME_PHASE_PIECE_VALUES,
    MIDGAME_BISHOP_VALUES, MIDGAME_KING_VALUES, MIDGAME_KNIGHT_VALUES, MIDGAME_PAWN_VALUES,
    MIDGAME_PIECE_VALUES, MIDGAME_QUEEN_VALUES, MIDGAME_ROOK_VALUES,
};

/**
 * Everything in here is based on https://www.chessprogramming.org/PeSTO%27s_Evaluation_Function
 */
mod constants;

pub struct PestoEvaluator {
    midgame_table: [[[i32; 64]; 6]; 2],
    endgame_table: [[[i32; 64]; 6]; 2],
}

impl PestoEvaluator {
    pub fn new() -> Self {
        Self {
            midgame_table: generate_midgame_table(),
            endgame_table: generate_endgame_table(),
        }
    }
}

impl Default for PestoEvaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl Evaluator for PestoEvaluator {
    fn evaluate(&self, board: &Board, for_color: usize) -> Score {
        let game_phase: u32 = GAME_PHASE_PIECE_VALUES[0]
            * (board.pawns[COLOR_WHITE] | board.pawns[COLOR_BLACK]).count_ones()
            + GAME_PHASE_PIECE_VALUES[1]
                * (board.knights[COLOR_WHITE] | board.knights[COLOR_BLACK]).count_ones()
            + GAME_PHASE_PIECE_VALUES[2]
                * (board.bishops[COLOR_WHITE] | board.bishops[COLOR_BLACK]).count_ones()
            + GAME_PHASE_PIECE_VALUES[3]
                * (board.rooks[COLOR_WHITE] | board.rooks[COLOR_BLACK]).count_ones()
            + GAME_PHASE_PIECE_VALUES[4]
                * (board.queens[COLOR_WHITE] | board.queens[COLOR_BLACK]).count_ones()
            + GAME_PHASE_PIECE_VALUES[5]
                * (board.king[COLOR_WHITE] | board.king[COLOR_BLACK]).count_ones();

        fn score(pieces: u64, values: &[i32; 64]) -> i32 {
            pieces.as_bit_index_iter().map(|i| values[i as usize]).sum()
        }

        fn score_color(board: &Board, color: usize, table: &[[[i32; 64]; 6]; 2]) -> i32 {
            score(board.pawns[color], &table[color][0])
                + score(board.knights[color], &table[color][1])
                + score(board.bishops[color], &table[color][2])
                + score(board.rooks[color], &table[color][3])
                + score(board.queens[color], &table[color][4])
                + score(board.king[color], &table[color][5])
        }

        let midgame_score = score_color(board, COLOR_WHITE, &self.midgame_table)
            - score_color(board, COLOR_BLACK, &self.midgame_table);
        let endgame_score = score_color(board, COLOR_WHITE, &self.endgame_table)
            - score_color(board, COLOR_BLACK, &self.endgame_table);

        let midgame_part = game_phase.min(24);
        let endgame_part = 24 - midgame_part;
        let white_score = Score::Value(
            (midgame_score * (midgame_part as i32) + endgame_score * (endgame_part as i32)) / 24,
        );
        if for_color == COLOR_WHITE {
            white_score
        } else {
            -white_score
        }
    }

    fn evaluate_move_by_board(&self, prev_board: &Board, board: &Board) -> i32 {
        if board.is_check() {
            // Checks go first!
            return -1_000;
        }

        let self_occupancy = board.occupancy_bits_for(board.active_color);
        let prev_self_occupancy = prev_board.occupancy_bits_for(board.active_color);

        if self_occupancy != prev_self_occupancy {
            let captured_mask = prev_self_occupancy & !self_occupancy;

            // What was captured?
            let captured_value = MIDGAME_PIECE_VALUES[0]
                * (captured_mask & prev_board.pawns[board.active_color]).count_ones() as i32
                + MIDGAME_PIECE_VALUES[1]
                    * (captured_mask & prev_board.knights[board.active_color]).count_ones() as i32
                + MIDGAME_PIECE_VALUES[2]
                    * (captured_mask & prev_board.bishops[board.active_color]).count_ones() as i32
                + MIDGAME_PIECE_VALUES[3]
                    * (captured_mask & prev_board.rooks[board.active_color]).count_ones() as i32
                + MIDGAME_PIECE_VALUES[4]
                    * (captured_mask & prev_board.queens[board.active_color]).count_ones() as i32;

            let capturer_value = MIDGAME_PIECE_VALUES[0]
                * (captured_mask & board.pawns[prev_board.active_color]).count_ones() as i32
                + MIDGAME_PIECE_VALUES[1]
                    * (captured_mask & board.knights[prev_board.active_color]).count_ones() as i32
                + MIDGAME_PIECE_VALUES[2]
                    * (captured_mask & board.bishops[prev_board.active_color]).count_ones() as i32
                + MIDGAME_PIECE_VALUES[3]
                    * (captured_mask & board.rooks[prev_board.active_color]).count_ones() as i32
                + MIDGAME_PIECE_VALUES[4]
                    * (captured_mask & board.queens[prev_board.active_color]).count_ones() as i32;

            // MVV-LVA
            return capturer_value - captured_value;
        }
        1_000
    }
}

fn generate_midgame_table() -> [[[i32; 64]; 6]; 2] {
    generate_piece_table(
        &MIDGAME_PIECE_VALUES,
        &[
            &MIDGAME_PAWN_VALUES,
            &MIDGAME_KNIGHT_VALUES,
            &MIDGAME_BISHOP_VALUES,
            &MIDGAME_ROOK_VALUES,
            &MIDGAME_QUEEN_VALUES,
            &MIDGAME_KING_VALUES,
        ],
    )
}

fn generate_endgame_table() -> [[[i32; 64]; 6]; 2] {
    generate_piece_table(
        &ENDGAME_PIECE_VALUES,
        &[
            &ENDGAME_PAWN_VALUES,
            &ENDGAME_KNIGHT_VALUES,
            &ENDGAME_BISHOP_VALUES,
            &ENDGAME_ROOK_VALUES,
            &ENDGAME_QUEEN_VALUES,
            &ENDGAME_KING_VALUES,
        ],
    )
}

fn generate_piece_table(
    piece_values: &[i32; 6],
    piece_square_values: &[&[i32; 64]; 6],
) -> [[[i32; 64]; 6]; 2] {
    let mut output: [[[i32; 64]; 6]; 2] = [[[0; 64]; 6]; 2];

    for piece_index in 0..6 {
        let piece_value = piece_values[piece_index];
        for square_index in 0..64 {
            output[COLOR_WHITE][piece_index][square_index] =
                piece_square_values[piece_index][square_index] + piece_value;

            // Squares for black pieces are reversed in rank
            let flipped_square_index = square_index ^ 56;
            output[COLOR_BLACK][piece_index][square_index] =
                piece_square_values[piece_index][flipped_square_index] + piece_value;
        }
    }

    output
}

#[cfg(test)]
mod tests {
    use crate::{
        board::{board_move::BoardMove, Board, COLOR_BLACK, COLOR_WHITE},
        evaluator::{
            pesto::constants::{ENDGAME_PIECE_VALUES, ENDGAME_QUEEN_VALUES},
            Evaluator,
        },
        score::Score,
    };

    use super::PestoEvaluator;

    #[test]
    fn test_piece_tables() {
        let evaluator = PestoEvaluator::new();

        // Check that the tables for black and white are flipped.
        assert_eq!(
            evaluator.midgame_table[0][1][1],
            evaluator.midgame_table[1][1][57]
        );
        assert_eq!(
            evaluator.midgame_table[0][2][10],
            evaluator.midgame_table[1][2][50]
        );

        // Check that the values make some sense.
        assert_eq!(
            evaluator.endgame_table[0][4][14],
            ENDGAME_PIECE_VALUES[4] + ENDGAME_QUEEN_VALUES[14]
        );
        assert_eq!(
            evaluator.endgame_table[1][3][0],
            ENDGAME_PIECE_VALUES[3] + ENDGAME_QUEEN_VALUES[0]
        );
    }

    #[test]
    fn test_basic_eval() {
        let evaluator = PestoEvaluator::new();

        assert_eq!(
            Score::Value(0),
            evaluator.evaluate(&Board::new_setup(), COLOR_WHITE)
        );

        let board = Board::new_setup();
        let kings_pawn = board.apply_board_move(&BoardMove::new(52, 36)).unwrap();
        let f_pawn = board.apply_board_move(&BoardMove::new(53, 37)).unwrap();

        // Good openings are good
        assert!(evaluator.evaluate(&kings_pawn, COLOR_WHITE) > Score::Value(0));

        // Bad openings are bad
        assert!(evaluator.evaluate(&f_pawn, COLOR_WHITE) < Score::Value(0));

        // Score for black is the negation of the score for white
        assert_eq!(
            evaluator.evaluate(&kings_pawn, COLOR_WHITE),
            -evaluator.evaluate(&kings_pawn, COLOR_BLACK)
        );
        assert_eq!(
            evaluator.evaluate(&f_pawn, COLOR_WHITE),
            -evaluator.evaluate(&f_pawn, COLOR_BLACK)
        );
    }
}
