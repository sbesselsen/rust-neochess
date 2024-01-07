use crate::{board::Board, score::Score};

pub mod default;

pub trait Evaluator {
    fn evaluate(&self, board: &Board, for_color: usize) -> Score;
    fn evaluate_move_by_board(&self, prev_board: &Board, board: &Board) -> i32;
}
