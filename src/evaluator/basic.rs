use crate::{
    board::{Board, COLOR_BLACK, COLOR_WHITE, RANK_0_MASK},
    evaluator::{Evaluator, Score},
};

#[derive(Debug, PartialEq, Eq)]
pub struct BasicEvaluator;

impl BasicEvaluator {
    pub fn new() -> Self {
        Self {}
    }

    fn evaluate_for_white(&self, board: &Board) -> Score {
        let white_is_dead = board.king[COLOR_WHITE] == 0;
        let black_is_dead = board.king[COLOR_BLACK] == 0;
        if white_is_dead && !black_is_dead {
            return Score::MinusInfinity;
        }
        if black_is_dead && !white_is_dead {
            return Score::PlusInfinity;
        }
        Score::Value(
            100 * (board.pawns[COLOR_WHITE].count_ones() as i32
                - board.pawns[COLOR_BLACK].count_ones() as i32)
                + 500
                    * (board.rooks[COLOR_WHITE].count_ones() as i32
                        - board.rooks[COLOR_BLACK].count_ones() as i32)
                + 300
                    * (board.knights[COLOR_WHITE].count_ones() as i32
                        - board.knights[COLOR_BLACK].count_ones() as i32)
                + 300
                    * (board.bishops[COLOR_WHITE].count_ones() as i32
                        - board.bishops[COLOR_BLACK].count_ones() as i32)
                + 900
                    * (board.queens[COLOR_WHITE].count_ones() as i32
                        - board.queens[COLOR_BLACK].count_ones() as i32),
        )
    }
}

impl Default for BasicEvaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl Evaluator for BasicEvaluator {
    fn evaluate(&self, board: &Board, for_color: usize) -> Score {
        let score = self.evaluate_for_white(board);
        if for_color == COLOR_WHITE {
            score
        } else {
            -score
        }
    }

    fn evaluate_move_by_board(&self, prev_board: &Board, board: &Board) -> i32 {
        if board.is_check() {
            // Checks go first!
            return -10;
        }

        let promotion_rank_mask = if prev_board.active_color == COLOR_WHITE {
            RANK_0_MASK << 48
        } else {
            RANK_0_MASK << 8
        };
        if (prev_board.pawns[prev_board.active_color] & !board.pawns[prev_board.active_color])
            & promotion_rank_mask
            > 0
        {
            // This is a promotion.
            return -5;
        }

        let self_occupancy = board.occupancy_bits_for(board.active_color);
        let prev_self_occupancy = prev_board.occupancy_bits_for(board.active_color);

        if self_occupancy != prev_self_occupancy {
            let captured_mask = prev_self_occupancy & !self_occupancy;

            // What was captured?
            let captured_value = (captured_mask & prev_board.pawns[board.active_color])
                .count_ones()
                + 5 * (captured_mask & prev_board.rooks[board.active_color]).count_ones()
                + 3 * (captured_mask & prev_board.knights[board.active_color]).count_ones()
                + 3 * (captured_mask & prev_board.bishops[board.active_color]).count_ones()
                + 9 * (captured_mask & prev_board.queens[board.active_color]).count_ones();

            let capturer_value = (captured_mask & board.pawns[prev_board.active_color])
                .count_ones()
                + 5 * (captured_mask & board.rooks[prev_board.active_color]).count_ones()
                + 3 * (captured_mask & board.knights[prev_board.active_color]).count_ones()
                + 3 * (captured_mask & board.bishops[prev_board.active_color]).count_ones()
                + 9 * (captured_mask & board.queens[prev_board.active_color]).count_ones();

            // MVV-LVA
            return (capturer_value as i32) - (captured_value as i32);
        }
        100
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        board::{Board, COLOR_BLACK, COLOR_WHITE},
        evaluator::{Evaluator, Score},
    };

    use super::BasicEvaluator;

    #[test]
    fn trivial_zeros() {
        let eval = BasicEvaluator::new();
        assert_eq!(
            eval.evaluate(&Board::new_setup(), COLOR_WHITE),
            Score::Value(0)
        );
        assert_eq!(eval.evaluate(&Board::new(), COLOR_WHITE), Score::Value(0));
    }

    #[test]
    fn dead_kings() {
        let eval = BasicEvaluator::new();
        let board = Board::new_setup().apply_mutation(|b| {
            b.king[COLOR_WHITE] = 0;
        });
        assert_eq!(eval.evaluate(&board, COLOR_BLACK), Score::PlusInfinity);
        let board = Board::new_setup().apply_mutation(|b| {
            b.king[COLOR_BLACK] = 0;
        });
        assert_eq!(eval.evaluate(&board, COLOR_WHITE), Score::PlusInfinity);
        let board = Board::new_setup().apply_mutation(|b| {
            b.king[COLOR_WHITE] = 0;
            b.king[COLOR_BLACK] = 0;
        });
        assert_eq!(eval.evaluate(&board, COLOR_BLACK), Score::Value(0));
    }

    #[test]
    fn eval_some_board() {
        let eval = BasicEvaluator::new();
        let board =
            Board::try_parse_fen("8/6p1/1N1kbp2/1p2pR2/6P1/3PBBNr/1P6/3K4 w - - 0 1").unwrap();
        assert_eq!(eval.evaluate(&board, COLOR_WHITE), Score::Value(800));
    }
}
