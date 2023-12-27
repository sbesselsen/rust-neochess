use crate::{
    bitboard::{BitBoard, COLOR_WHITE},
    evaluator::{DefaultEvaluator, Evaluator, EvaluatorScore},
};

#[derive(Debug)]
pub struct Engine<E> {
    evaluator: E,
}

impl Default for Engine<DefaultEvaluator> {
    fn default() -> Self {
        Self::new()
    }
}

impl<E> Engine<E>
where
    E: Evaluator,
{
    pub fn new_with_evaluator(evaluator: E) -> Self {
        Engine { evaluator }
    }
}

impl Engine<DefaultEvaluator> {
    pub fn new() -> Self {
        Engine {
            evaluator: Default::default(),
        }
    }

    pub fn minmax(&self, board: &BitBoard, depth: u32) -> Option<(BitBoard, EvaluatorScore)> {
        assert!(depth > 0, "depth should be at least 1");
        let boards_with_scores = board
            .next_boards()
            .into_iter()
            .filter_map(|b| self.minmax_eval(&b, depth - 1).map(|eval| (b, eval)));
        if board.active_color == COLOR_WHITE {
            boards_with_scores.max_by(|x, y| x.1.cmp(&y.1))
        } else {
            boards_with_scores.min_by(|x, y| x.1.cmp(&y.1))
        }
    }

    fn minmax_eval(&self, board: &BitBoard, depth: u32) -> Option<EvaluatorScore> {
        if depth == 0 {
            return Some(self.evaluator.evaluate(board));
        }

        let next_boards = board.next_boards();
        if next_boards.is_empty() {
            if board.is_check() {
                return Some(EvaluatorScore::infinity_for(board.active_color).inverse());
            } else {
                return Some(EvaluatorScore::Value(0.0));
            }
        }

        let scores = next_boards
            .into_iter()
            .filter_map(|b| self.minmax_eval(&b, depth - 1));

        if board.active_color == COLOR_WHITE {
            scores.max()
        } else {
            scores.min()
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{bitboard::BitBoard, engine::DefaultEvaluator, evaluator::EvaluatorScore};

    use super::Engine;

    #[test]
    fn create_engine() {
        let engine = Engine::new();
        assert_eq!(engine.evaluator, DefaultEvaluator::new());
    }

    #[test]
    fn simple_checkmate() {
        let board = BitBoard::try_parse_fen(
            "r3kbnr/p1pp1ppp/bp2P3/8/1n5N/4P3/PPP2PPP/RNBQKB1R w KQkq - 1 7",
        )
        .unwrap();

        let engine = Engine::new();
        let result = engine.minmax(&board, 3);

        assert!(result.is_some());

        let (_, score) = result.unwrap();

        // The engine notices this is checkmate.
        assert_eq!(score, EvaluatorScore::PlusInfinity);
    }

    #[test]
    fn mate_in_2_minmax() {
        // Taken from https://wtharvey.com/m8n2.txt
        let board = BitBoard::try_parse_fen(
            "r2qkb1r/pp2nppp/3p4/2pNN1B1/2BnP3/3P4/PPP2PPP/R2bK2R w KQkq - 1 1",
        )
        .unwrap();

        let engine = Engine::new();
        let result = engine.minmax(&board, 4);

        assert!(result.is_some());

        let (board2, score) = result.unwrap();

        // The engine notices this is checkmate.
        assert_eq!(score, EvaluatorScore::PlusInfinity);

        // It got the right move.
        assert_eq!(
            board2.to_fen(),
            "r2qkb1r/pp2nppp/3p1N2/2p1N1B1/2BnP3/3P4/PPP2PPP/R2bK2R b KQkq - 2 1"
        );
    }
}
