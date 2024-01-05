use std::{cmp::Ordering, fmt::Display, ops::Neg};

use crate::bitboard::{BitBoard, COLOR_BLACK, COLOR_WHITE, RANK_0_MASK};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum EvaluatorScore {
    MinusInfinity,
    Value(f64),
    PlusInfinity,
}

impl EvaluatorScore {
    pub fn is_finite(&self) -> bool {
        matches!(self, EvaluatorScore::Value(_))
    }

    pub fn is_win(&self, color: usize) -> bool {
        match self {
            EvaluatorScore::PlusInfinity => color == COLOR_WHITE,
            EvaluatorScore::MinusInfinity => color == COLOR_BLACK,
            _ => false,
        }
    }
}

impl Neg for EvaluatorScore {
    type Output = EvaluatorScore;

    fn neg(self) -> Self::Output {
        match self {
            EvaluatorScore::MinusInfinity => EvaluatorScore::PlusInfinity,
            EvaluatorScore::PlusInfinity => EvaluatorScore::MinusInfinity,
            EvaluatorScore::Value(v) => EvaluatorScore::Value(-v),
        }
    }
}

impl std::hash::Hash for EvaluatorScore {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            EvaluatorScore::Value(x) => state.write_i64((x * 1_000_000.0) as i64),
            EvaluatorScore::MinusInfinity => {
                state.write_u8(1);
            }
            EvaluatorScore::PlusInfinity => {
                state.write_u8(2);
            }
        }
        state.finish();
    }
}

/* Don't be putting any NaN in my values okay? */
impl Eq for EvaluatorScore {}

impl PartialOrd for EvaluatorScore {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for EvaluatorScore {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self == other {
            return Ordering::Equal;
        }
        match (self, other) {
            (EvaluatorScore::MinusInfinity, EvaluatorScore::MinusInfinity) => Ordering::Equal,
            (EvaluatorScore::MinusInfinity, _) => Ordering::Less,
            (_, EvaluatorScore::MinusInfinity) => Ordering::Greater,
            (EvaluatorScore::PlusInfinity, EvaluatorScore::PlusInfinity) => Ordering::Equal,
            (EvaluatorScore::PlusInfinity, _) => Ordering::Greater,
            (_, EvaluatorScore::PlusInfinity) => Ordering::Less,
            (EvaluatorScore::Value(x), EvaluatorScore::Value(y)) => {
                if x < y {
                    Ordering::Less
                } else if x > y {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            }
        }
    }
}

impl Display for EvaluatorScore {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvaluatorScore::MinusInfinity => f.write_str("-Infinity"),
            EvaluatorScore::PlusInfinity => f.write_str("+Infinity"),
            EvaluatorScore::Value(x) => f.write_fmt(format_args!("{}", x)),
        }
    }
}

pub trait Evaluator {
    fn evaluate(&self, board: &BitBoard) -> EvaluatorScore;
    fn order_moves(&self, prev_board: &BitBoard, boards: &mut Vec<BitBoard>);
}

#[derive(Debug, PartialEq, Eq)]
pub struct DefaultEvaluator;

impl DefaultEvaluator {
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for DefaultEvaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl Evaluator for DefaultEvaluator {
    fn evaluate(&self, board: &BitBoard) -> EvaluatorScore {
        let white_is_dead = board.king[COLOR_WHITE] == 0;
        let black_is_dead = board.king[COLOR_BLACK] == 0;
        if white_is_dead && !black_is_dead {
            return EvaluatorScore::MinusInfinity;
        }
        if black_is_dead && !white_is_dead {
            return EvaluatorScore::PlusInfinity;
        }
        EvaluatorScore::Value(
            (board.pawns[COLOR_WHITE].count_ones() as f64
                - board.pawns[COLOR_BLACK].count_ones() as f64)
                + 5.0
                    * (board.rooks[COLOR_WHITE].count_ones() as f64
                        - board.rooks[COLOR_BLACK].count_ones() as f64)
                + 3.0
                    * (board.knights[COLOR_WHITE].count_ones() as f64
                        - board.knights[COLOR_BLACK].count_ones() as f64)
                + 3.0
                    * (board.bishops[COLOR_WHITE].count_ones() as f64
                        - board.bishops[COLOR_BLACK].count_ones() as f64)
                + 9.0
                    * (board.queens[COLOR_WHITE].count_ones() as f64
                        - board.queens[COLOR_BLACK].count_ones() as f64),
        )
    }

    fn order_moves(&self, prev_board: &BitBoard, boards: &mut Vec<BitBoard>) {
        boards.sort_by_cached_key(|b| {
            if b.is_check() {
                // Checks go first!
                return -10;
            }

            let promotion_rank_mask = if prev_board.active_color == COLOR_WHITE {
                RANK_0_MASK << 48
            } else {
                RANK_0_MASK << 8
            };
            if (prev_board.pawns[prev_board.active_color] & !b.pawns[prev_board.active_color])
                & promotion_rank_mask
                > 0
            {
                // This is a promotion.
                return -5;
            }

            let self_occupancy = b.occupancy_bits_for(b.active_color);
            let prev_self_occupancy = prev_board.occupancy_bits_for(b.active_color);

            if self_occupancy != prev_self_occupancy {
                let captured_mask = prev_self_occupancy & !self_occupancy;

                // What was captured?
                let captured_value = (captured_mask & prev_board.pawns[b.active_color])
                    .count_ones()
                    + 5 * (captured_mask & prev_board.rooks[b.active_color]).count_ones()
                    + 3 * (captured_mask & prev_board.knights[b.active_color]).count_ones()
                    + 3 * (captured_mask & prev_board.bishops[b.active_color]).count_ones()
                    + 9 * (captured_mask & prev_board.queens[b.active_color]).count_ones();

                let capturer_value = (captured_mask & b.pawns[prev_board.active_color])
                    .count_ones()
                    + 5 * (captured_mask & b.rooks[prev_board.active_color]).count_ones()
                    + 3 * (captured_mask & b.knights[prev_board.active_color]).count_ones()
                    + 3 * (captured_mask & b.bishops[prev_board.active_color]).count_ones()
                    + 9 * (captured_mask & b.queens[prev_board.active_color]).count_ones();

                // MVV-LVA
                return (capturer_value - captured_value) as i32;
            }
            100
        });
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        bitboard::{BitBoard, COLOR_BLACK, COLOR_WHITE},
        evaluator::{Evaluator, EvaluatorScore},
    };

    use super::DefaultEvaluator;

    #[test]
    fn trivial_zeros() {
        let eval = DefaultEvaluator::new();
        assert_eq!(
            eval.evaluate(&BitBoard::new_setup()),
            EvaluatorScore::Value(0.0)
        );
        assert_eq!(eval.evaluate(&BitBoard::new()), EvaluatorScore::Value(0.0));
    }

    #[test]
    fn dead_kings() {
        let eval = DefaultEvaluator::new();
        let board = BitBoard::new_setup().apply_move(|b| {
            b.king[COLOR_WHITE] = 0;
        });
        assert_eq!(eval.evaluate(&board), EvaluatorScore::MinusInfinity);
        let board = BitBoard::new_setup().apply_move(|b| {
            b.king[COLOR_BLACK] = 0;
        });
        assert_eq!(eval.evaluate(&board), EvaluatorScore::PlusInfinity);
        let board = BitBoard::new_setup().apply_move(|b| {
            b.king[COLOR_WHITE] = 0;
            b.king[COLOR_BLACK] = 0;
        });
        assert_eq!(eval.evaluate(&board), EvaluatorScore::Value(0.0));
    }

    #[test]
    fn eval_some_board() {
        let eval = DefaultEvaluator::new();
        let board =
            BitBoard::try_parse_fen("8/6p1/1N1kbp2/1p2pR2/6P1/3PBBNr/1P6/3K4 w - - 0 1").unwrap();
        assert_eq!(eval.evaluate(&board), EvaluatorScore::Value(8.0));
    }
}
