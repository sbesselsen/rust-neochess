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

    pub fn minmax_cutoff(
        &self,
        board: &BitBoard,
        depth: u32,
    ) -> (Option<BitBoard>, EvaluatorScore) {
        debug_assert!(depth > 0, "depth should be at least 1");
        self.minmax_cutoff_inner(
            board,
            depth,
            EvaluatorScore::MinusInfinity,
            EvaluatorScore::PlusInfinity,
        )
    }

    fn minmax_cutoff_inner(
        &self,
        board: &BitBoard,
        depth: u32,
        best_enforced_for_white: EvaluatorScore,
        best_enforced_for_black: EvaluatorScore,
    ) -> (Option<BitBoard>, EvaluatorScore) {
        if depth == 0 {
            return (None, self.evaluator.evaluate(board));
        }

        let next_boards = board.next_boards();
        if next_boards.is_empty() {
            if board.is_check() {
                return (
                    None,
                    EvaluatorScore::infinity_for(board.active_color).inverse(),
                );
            } else {
                return (None, EvaluatorScore::Value(0.0));
            }
        }

        let mut best_enforced_for_white = best_enforced_for_white;
        let mut best_enforced_for_black = best_enforced_for_black;
        let mut best_board: Option<BitBoard> = None;
        let mut best_score: EvaluatorScore;

        if board.active_color == COLOR_WHITE {
            best_score = EvaluatorScore::MinusInfinity;
            for b in next_boards {
                let (_, score) = self.minmax_cutoff_inner(
                    &b,
                    depth - 1,
                    best_enforced_for_white,
                    best_enforced_for_black,
                );
                if score > best_score {
                    best_board = Some(b);
                    best_score = score;
                }
                best_enforced_for_white = best_enforced_for_white.max(score);
                if score >= best_enforced_for_black {
                    // Beta cutoff
                    break;
                }
            }
            (best_board, best_score)
        } else {
            best_score = EvaluatorScore::PlusInfinity;
            for b in next_boards {
                let (_, score) = self.minmax_cutoff_inner(
                    &b,
                    depth - 1,
                    best_enforced_for_white,
                    best_enforced_for_black,
                );
                if score < best_score {
                    best_board = Some(b);
                    best_score = score;
                }
                best_enforced_for_black = best_enforced_for_black.min(score);
                if score <= best_enforced_for_white {
                    // Alpha cutoff
                    break;
                }
            }
            (best_board, best_score)
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
    fn mate_in_1() {
        let board = BitBoard::try_parse_fen(
            "r3kbnr/p1pp1ppp/bp2P3/8/1n5N/4P3/PPP2PPP/RNBQKB1R w KQkq - 1 7",
        )
        .unwrap();

        let engine = Engine::new();
        let (b, score) = engine.minmax_cutoff(&board, 3);

        // The engine notices this is checkmate.
        assert_eq!(score, EvaluatorScore::PlusInfinity);

        assert!(b.is_some());
        assert_eq!(
            b.unwrap().to_fen(),
            "r3kbnr/p1pQ1ppp/bp2P3/8/1n5N/4P3/PPP2PPP/RNB1KB1R b KQkq - 0 7"
        );
    }

    #[test]
    fn mate_in_2() {
        // Taken from https://wtharvey.com/m8n2.txt
        let board = BitBoard::try_parse_fen(
            "r2qkb1r/pp2nppp/3p4/2pNN1B1/2BnP3/3P4/PPP2PPP/R2bK2R w KQkq - 1 1",
        )
        .unwrap();

        let engine = Engine::new();
        let (b, score) = engine.minmax_cutoff(&board, 4);

        // The engine notices this is checkmate.
        assert_eq!(score, EvaluatorScore::PlusInfinity);

        // It got the right move.
        assert!(b.is_some());
        assert_eq!(
            b.unwrap().to_fen(),
            "r2qkb1r/pp2nppp/3p1N2/2p1N1B1/2BnP3/3P4/PPP2PPP/R2bK2R b KQkq - 0 1"
        );
    }
}
