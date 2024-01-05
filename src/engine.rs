use std::fmt::Debug;

use crate::{
    board::{Board, BoardMove, COLOR_WHITE},
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

#[derive(Copy, Clone, Debug, PartialEq)]
enum TranspositionTableBound {
    Exact,
    Upper,
    Lower,
}

impl Eq for TranspositionTableBound {}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct TranspositionTableEntry {
    zobrist_hash: u64,
    depth: u32,
    score: EvaluatorScore,
    bound: TranspositionTableBound,
    best_move: Option<BoardMove>,
}

pub struct Engine {
    evaluator: Box<dyn Evaluator>,
    transposition_table: Vec<Option<TranspositionTableEntry>>,
    stats: EngineStats,
}

impl Debug for Engine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Engine")
            .field("evaluator", &format_args!("(boxed)"))
            .field("transposition_table", &self.transposition_table)
            .finish()
    }
}
#[derive(Copy, Clone, Debug, Default)]
pub struct EngineStats {
    nodes: u32,
    leaves: u32,
}

impl From<EngineBuilder> for Engine {
    fn from(builder: EngineBuilder) -> Self {
        let evaluator = builder
            .evaluator
            .unwrap_or_else(|| Box::new(DefaultEvaluator::new()));

        let transposition_table_size = builder.transposition_table_size.unwrap_or(1_000_000);
        let mut transposition_table: Vec<Option<TranspositionTableEntry>> =
            Vec::with_capacity(transposition_table_size);
        transposition_table.resize_with(transposition_table_size, || None);

        Engine {
            evaluator,
            transposition_table,
            stats: EngineStats::default(),
        }
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::from(EngineBuilder::default())
    }
}

impl Engine {
    pub fn search(&mut self, board: &Board, depth: u32) -> (Option<Board>, EvaluatorScore) {
        assert!(depth > 0, "depth should be at least 1");

        // Reset stats
        self.stats = EngineStats::default();

        let (mv, score) = self.search_inner(
            board,
            depth,
            false,
            EvaluatorScore::MinusInfinity,
            EvaluatorScore::PlusInfinity,
        );
        let board_after_move = mv.map(|mv| {
            board
                .apply_board_move(&mv)
                .expect("engine-generated move should be applicable")
        });
        if board.active_color == COLOR_WHITE {
            (board_after_move, score)
        } else {
            (board_after_move, -score)
        }
    }

    pub fn stats(&self) -> EngineStats {
        self.stats
    }

    fn search_inner(
        &mut self,
        board: &Board,
        depth: u32,
        allow_null: bool,
        alpha: EvaluatorScore,
        beta: EvaluatorScore,
    ) -> (Option<BoardMove>, EvaluatorScore) {
        self.stats.nodes += 1;

        let alpha_orig = alpha;
        let mut alpha = alpha;
        let mut beta = beta;

        let tt_index = (board.zobrist_hash % (self.transposition_table.len() as u64)) as usize;
        let tt_entry =
            self.transposition_table[tt_index].filter(|e| e.zobrist_hash == board.zobrist_hash);

        if let Some(entry) = tt_entry {
            if entry.depth >= depth {
                // The score was calculated to at least the depth we need.
                match entry.bound {
                    TranspositionTableBound::Exact => {
                        // We know the exact score. We can return it!
                        return (entry.best_move, entry.score);
                    }
                    TranspositionTableBound::Lower => {
                        // We know a lower bound for this board.
                        alpha = alpha.max(entry.score);
                    }
                    TranspositionTableBound::Upper => {
                        // We know an upper bound for this board.
                        // Update beta so we can terminate deeper nodes if they
                        // appear to be better than the upper bound.
                        beta = beta.min(entry.score);
                    }
                }
                if alpha >= beta {
                    return (entry.best_move, entry.score);
                }
            }
        }

        if depth == 0 {
            self.stats.leaves += 1;
            return (None, self.evaluator.evaluate(board, board.active_color));
        }

        // Null move pruning
        let null_move_depth_reduction = 2;
        if allow_null && depth > null_move_depth_reduction + 1 && !board.is_check() {
            let null_move_board = board.apply_mutation(|_| {});
            let (_, null_move_score) = self.search_inner(
                &null_move_board,
                depth - null_move_depth_reduction - 1,
                false,
                -beta,
                -alpha,
            );
            let null_move_score = -null_move_score;
            if null_move_score >= beta {
                // Null move pruning
                return (None, beta);
            }
        }

        let mut next_boards = board.next_boards();

        // Order moves: best move from transposition table goes first, then order heuristically.
        let tt_best_board = tt_entry
            .map(|e| e.best_move.map(|mv| board.apply_board_move(&mv).ok()))
            .flatten()
            .flatten();
        next_boards.sort_by_cached_key(|b| {
            if let Some(tt_best_board) = &tt_best_board {
                if b == tt_best_board {
                    return -1000;
                }
            }
            self.evaluator.evaluate_move_by_board(board, b)
        });

        if next_boards.is_empty() {
            let score = if board.is_check() {
                EvaluatorScore::MinusInfinity
            } else {
                EvaluatorScore::Value(0)
            };
            return (None, score);
        }

        let mut best_board: Option<Board> = None;
        let mut best_score = EvaluatorScore::MinusInfinity;

        for b in next_boards {
            let (_, score) = self.search_inner(&b, depth - 1, true, -beta, -alpha);
            let score = -score;
            if score > best_score || best_board.is_none() {
                best_board = Some(b);
                best_score = score;
                alpha = alpha.max(score);
            }
            if alpha >= beta {
                // Cutoff: best score for the current player is better than the best guaranteed score for the other player.
                // The game will never go down this path (and if it will, that's a bonus).

                // Store this value in the transposition table, but note that it is not exact.
                // Instead, it is a lower bound on what the real value will be.
                break;
            }
        }

        // Add to transposition table.
        let bound = if best_score <= alpha_orig {
            TranspositionTableBound::Upper
        } else if best_score >= beta {
            TranspositionTableBound::Lower
        } else {
            TranspositionTableBound::Exact
        };
        let best_move = if let Some(best_board) = &best_board {
            best_board.as_board_move(board)
        } else {
            None
        };

        if self.transposition_table[tt_index]
            .map(|e| e.zobrist_hash != board.zobrist_hash || e.depth <= depth)
            .unwrap_or(true)
        {
            // There is no entry in the table for this board, or it is worse in depth than what we have.
            self.transposition_table[tt_index].replace(TranspositionTableEntry {
                zobrist_hash: board.zobrist_hash,
                depth,
                score: best_score,
                bound,
                best_move,
            });
        }

        (best_move, best_score)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        bitwise_helper::BitwiseHelper,
        board::{Board, COLOR_BLACK, COLOR_WHITE},
        evaluator::EvaluatorScore,
    };

    use super::Engine;

    #[test]
    fn create_engine() {
        // Just make sure it doesn't panic.
        let _engine = Engine::default();
    }

    #[test]
    fn mate_in_1() {
        let board =
            Board::try_parse_fen("r3kbnr/p1pp1ppp/bp2P3/8/1n5N/4P3/PPP2PPP/RNBQKB1R w KQkq - 1 7")
                .unwrap();

        let mut engine = Engine::default();
        let (b, score) = engine.search(&board, 3);

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
        let board = Board::try_parse_fen(
            "r2qkb1r/pp2nppp/3p4/2pNN1B1/2BnP3/3P4/PPP2PPP/R2bK2R w KQkq - 1 1",
        )
        .unwrap();

        let mut engine = Engine::default();
        let (b, score) = engine.search(&board, 4);

        // The engine notices this is checkmate.
        assert_eq!(score, EvaluatorScore::PlusInfinity);

        // It got the right move.
        assert!(b.is_some());
        assert_eq!(
            b.unwrap().to_fen(),
            "r2qkb1r/pp2nppp/3p1N2/2p1N1B1/2BnP3/3P4/PPP2PPP/R2bK2R b KQkq - 2 1"
        );
    }

    #[test]
    fn perf_test_1() {
        let board = Board::try_parse_fen("5r2/8/1R6/ppk3p1/2N3P1/P4b2/1K6/5B2 w - - 0 1").unwrap();

        let mut engine = Engine::default();
        let (b, _score) = engine.search(&board, 8);

        // It got the right move.
        assert_eq!(
            b.map(|b| b.to_fen()),
            Some(String::from("5r2/8/8/pRk3p1/2N3P1/P4b2/1K6/5B2 b - - 0 1")),
        );
    }

    #[test]
    fn perf_test_2() {
        let board =
            Board::try_parse_fen("5rk1/p1nnqr1p/1p1p4/3Pp2Q/5p1N/1P4PB/P2R1P1P/4R1K1 w - - 0 1")
                .unwrap();

        let mut engine = Engine::default();
        let (b, _score) = engine.search(&board, 8);

        // It got the right move.
        assert_eq!(
            b.map(|b| b.to_fen()),
            Some(String::from(
                "5rk1/p1nnqr1p/1p1p4/3PpN1Q/5p2/1P4PB/P2R1P1P/4R1K1 b - - 1 1"
            )),
        );
    }

    #[test]
    fn perf_test_3() {
        let board =
            Board::try_parse_fen("2r3k1/6r1/p3p3/3bQp1p/2pP2q1/P1P5/2B1R1PP/5RK1 b - - 0 1")
                .unwrap();

        let mut engine = Engine::default();
        let (b, _score) = engine.search(&board, 8);

        // It got the right move.
        assert_eq!(
            b.map(|b| b.to_fen()),
            Some(String::from(
                "2r3k1/6r1/p3p3/3bQp1p/2pP4/P1P5/2B1R1qP/5RK1 w - - 0 2"
            )),
        );
    }

    #[test]
    fn test_puzzle_1() {
        let board =
            Board::try_parse_fen("8/1kq3r1/p1p1b1N1/2p1Q2p/P2p4/3P2P1/1PP4P/4R1K1 b - - 0 1")
                .unwrap();

        let mut engine = Engine::default();

        let (b, _score) = engine.search(&board, 9);

        assert!(b.is_some());

        assert_eq!(
            b.unwrap().as_move_string(&board),
            Some(String::from("Rxg6"))
        );
    }

    #[test]
    fn test_puzzle_2() {
        let board = Board::try_parse_fen("3rkbnr/pp2pppp/8/q7/Q4B2/2N2P2/PPP2P1P/R3K2R b KQ - 0 1")
            .unwrap();

        let mut engine = Engine::default();

        // TODO: this fails if we increase the depth to 8!
        let (b, _score) = engine.search(&board, 6);

        assert!(b.is_some());
        let b = b.unwrap();

        assert_eq!(b.as_move_string(&board), Some(String::from("Qxa4")));

        let board = b.apply_mutation(|b| {
            b.knights[COLOR_WHITE].move_bit(21, 31);
            b.queens[COLOR_BLACK] = 0;
        });

        let (b, _score) = engine.search(&board, 6);

        assert!(b.is_some());
        let b = b.unwrap();

        assert_eq!(b.as_move_string(&board), Some(String::from("Rd4")));
    }
}
