use std::fmt::Debug;

use crate::{
    bitboard::{BitBoard, COLOR_WHITE},
    evaluator::{DefaultEvaluator, Evaluator, EvaluatorScore},
};

#[derive(Default)]
pub struct EngineBuilder {
    pub transposition_table_size: Option<usize>,
    pub evaluator: Option<Box<dyn Evaluator>>,
}

impl EngineBuilder {
    pub fn new() -> EngineBuilder {
        Default::default()
    }

    pub fn with_evaluator(self, evaluator: Box<dyn Evaluator>) -> Self {
        Self {
            evaluator: Some(evaluator),
            ..self
        }
    }

    pub fn with_transposition_table_size(self, size: usize) -> Self {
        Self {
            transposition_table_size: Some(size),
            ..self
        }
    }

    pub fn build(self) -> Engine {
        Engine::from(self)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct TranspositionTableEntry {
    zobrist_hash: u64,
    depth: u32,
    score: EvaluatorScore,
    bounds: (EvaluatorScore, EvaluatorScore),
}

pub struct Engine {
    evaluator: Box<dyn Evaluator>,
    transposition_table: Vec<Option<TranspositionTableEntry>>,
}

impl Debug for Engine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Engine")
            .field("evaluator", &format_args!("(boxed)"))
            .field("transposition_table", &self.transposition_table)
            .finish()
    }
}

impl From<EngineBuilder> for Engine {
    fn from(builder: EngineBuilder) -> Self {
        let evaluator = builder
            .evaluator
            .unwrap_or_else(|| Box::new(DefaultEvaluator::new()));

        let transposition_table_size = builder.transposition_table_size.unwrap_or(10_000_000);
        let mut transposition_table: Vec<Option<TranspositionTableEntry>> =
            Vec::with_capacity(transposition_table_size);
        transposition_table.resize_with(transposition_table_size, || None);

        Engine {
            evaluator,
            transposition_table,
        }
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::from(EngineBuilder::default())
    }
}

impl Engine {
    pub fn minmax_cutoff(
        &mut self,
        board: &BitBoard,
        depth: u32,
    ) -> (Option<BitBoard>, EvaluatorScore) {
        assert!(depth > 0, "depth should be at least 1");
        self.minmax_cutoff_inner(
            board,
            depth,
            true,
            EvaluatorScore::MinusInfinity,
            EvaluatorScore::PlusInfinity,
        )
    }

    fn minmax_cutoff_inner(
        &mut self,
        board: &BitBoard,
        depth: u32,
        return_board: bool,
        best_enforced_for_white: EvaluatorScore,
        best_enforced_for_black: EvaluatorScore,
    ) -> (Option<BitBoard>, EvaluatorScore) {
        if !return_board {
            if let Some(TranspositionTableEntry { score, .. }) = self.get_transposition_entry(
                board,
                depth,
                (best_enforced_for_white, best_enforced_for_black),
            ) {
                return (None, score);
            }
        }

        if depth == 0 {
            let score = self.evaluator.evaluate(board);
            self.add_transposition_entry(
                board,
                depth,
                score,
                (best_enforced_for_white, best_enforced_for_black),
            );
            return (None, score);
        }

        let next_boards = board.next_boards();
        if next_boards.is_empty() {
            let score = if board.is_check() {
                EvaluatorScore::infinity_for(board.active_color).inverse()
            } else {
                EvaluatorScore::Value(0.0)
            };
            self.add_transposition_entry(
                board,
                depth,
                score,
                (best_enforced_for_white, best_enforced_for_black),
            );
            return (None, score);
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
                    false,
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
            self.add_transposition_entry(
                board,
                depth,
                best_score,
                (best_enforced_for_white, best_enforced_for_black),
            );
        } else {
            best_score = EvaluatorScore::PlusInfinity;
            for b in next_boards {
                let (_, score) = self.minmax_cutoff_inner(
                    &b,
                    depth - 1,
                    false,
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
            self.add_transposition_entry(
                board,
                depth,
                best_score,
                (best_enforced_for_white, best_enforced_for_black),
            );
        }

        (best_board, best_score)
    }

    fn add_transposition_entry(
        &mut self,
        board: &BitBoard,
        depth: u32,
        score: EvaluatorScore,
        bounds: (EvaluatorScore, EvaluatorScore),
    ) {
        let index = (board.zobrist_hash % (self.transposition_table.len() as u64)) as usize;

        if matches!(
            self.transposition_table[index],
            Some(e) if e.depth > depth && e.zobrist_hash == board.zobrist_hash
        ) {
            // There is already a better entry in the table, computed to a greater depth.
            return;
        }

        // Write the new value.
        self.transposition_table[index].replace(TranspositionTableEntry {
            zobrist_hash: board.zobrist_hash,
            depth,
            score,
            bounds,
        });
    }

    fn get_transposition_entry(
        &self,
        board: &BitBoard,
        depth: u32,
        bounds: (EvaluatorScore, EvaluatorScore),
    ) -> Option<TranspositionTableEntry> {
        let index = (board.zobrist_hash % (self.transposition_table.len() as u64)) as usize;

        self.transposition_table[index].filter(|e| {
            e.depth >= depth
                && e.zobrist_hash == board.zobrist_hash
                && e.bounds.0 <= bounds.0
                && e.bounds.1 >= bounds.1
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{bitboard::BitBoard, evaluator::EvaluatorScore};

    use super::Engine;

    #[test]
    fn create_engine() {
        // Just make sure it doesn't panic.
        let _engine = Engine::default();
    }

    #[test]
    fn mate_in_1() {
        let board = BitBoard::try_parse_fen(
            "r3kbnr/p1pp1ppp/bp2P3/8/1n5N/4P3/PPP2PPP/RNBQKB1R w KQkq - 1 7",
        )
        .unwrap();

        let mut engine = Engine::default();
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

        let mut engine = Engine::default();
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

    #[test]
    fn transposition_test() {
        // Taken from https://wtharvey.com/m8n2.txt
        let board =
            BitBoard::try_parse_fen("5r2/8/1R6/ppk3p1/2N3P1/P4b2/1K6/5B2 w - - 0 1").unwrap();

        let mut engine = Engine::default();
        let (b, _score) = engine.minmax_cutoff(&board, 8);

        // It got the right move.
        assert_eq!(
            b.map(|b| b.to_fen()),
            Some(String::from("5r2/8/8/pRk3p1/2N3P1/P4b2/1K6/5B2 b - - 1 1")),
        );
    }
}
