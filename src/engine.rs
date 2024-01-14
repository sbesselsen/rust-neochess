use std::fmt::Debug;

use crate::{
    board::{board_move::BoardMove, Board, COLOR_WHITE},
    book::{EmptyOpeningBook, OpeningBook},
    evaluator::{pesto::PestoEvaluator, Evaluator},
    score::Score,
    threading::{CancelHandle, CancelSignal, InterruptableResult, InterruptedError},
};

// TODO: do something sensible with this
const OPENING_BOOK_WEIGHT_THRESHOLD: u16 = 10;

#[derive(Default)]
pub struct EngineBuilder {
    pub transposition_table_index_bits: Option<u8>,
    pub evaluator: Option<Box<dyn Evaluator + Send>>,
    pub opening_book: Option<Box<dyn OpeningBook + Send>>,
}

impl EngineBuilder {
    pub fn new() -> EngineBuilder {
        Default::default()
    }

    pub fn with_evaluator(self, evaluator: Box<dyn Evaluator + Send>) -> Self {
        Self {
            evaluator: Some(evaluator),
            ..self
        }
    }

    pub fn with_opening_book(self, opening_book: Box<dyn OpeningBook + Send>) -> Self {
        Self {
            opening_book: Some(opening_book),
            ..self
        }
    }

    pub fn with_transposition_table_index_bits(self, bits: u8) -> Self {
        Self {
            transposition_table_index_bits: Some(bits),
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
    score: Score,
    bound: TranspositionTableBound,
    best_move: Option<BoardMove>,
}

pub struct Engine {
    evaluator: Box<dyn Evaluator + Send>,
    opening_book: Box<dyn OpeningBook + Send>,
    transposition_table: Vec<Option<TranspositionTableEntry>>,
    transposition_table_index_bits: u8,
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
    quiescence_nodes: u32,
    leaves: u32,
}

impl From<EngineBuilder> for Engine {
    fn from(builder: EngineBuilder) -> Self {
        let evaluator = builder
            .evaluator
            .unwrap_or_else(|| Box::new(PestoEvaluator::new()));

        let opening_book = builder
            .opening_book
            .unwrap_or_else(|| Box::new(EmptyOpeningBook::new()));

        let transposition_table_index_bits = builder.transposition_table_index_bits.unwrap_or(20);
        assert!(
            transposition_table_index_bits < 30,
            "should not use more than 1 GB for the transposition table; no gains to be had"
        );
        let transposition_table_size = 2usize.pow(transposition_table_index_bits as u32);
        let mut transposition_table: Vec<Option<TranspositionTableEntry>> =
            Vec::with_capacity(transposition_table_size);
        transposition_table.resize(transposition_table_size, None);

        Engine {
            evaluator,
            opening_book,
            transposition_table,
            transposition_table_index_bits,
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
    pub fn reset_stats(&mut self) {
        self.stats = Default::default();
    }

    pub fn reset_state(&mut self) {
        self.transposition_table.fill(None);
    }

    pub fn search(&mut self, board: &Board, depth: u32) -> (Option<Board>, Score) {
        self.cancelable_search(board, depth, &CancelHandle::new().signal())
            .expect("search should not be canceled")
    }

    #[allow(clippy::result_large_err)]
    pub fn cancelable_search(
        &mut self,
        board: &Board,
        depth: u32,
        cancel_signal: &CancelSignal,
    ) -> InterruptableResult<(Option<Board>, Score)> {
        assert!(depth > 0, "depth should be at least 1");

        // Reset stats
        self.stats = EngineStats::default();

        let mut book_moves = self.opening_book.find(board.zobrist_hash);
        book_moves.retain(|e| e.weight >= OPENING_BOOK_WEIGHT_THRESHOLD);
        if !book_moves.is_empty() {
            let mv = book_moves[0].board_move;
            if let Ok(board_after_move) = board.apply_board_move(&mv) {
                // TODO: do something sensible with the score here
                return Ok((Some(board_after_move), Score::Value(50)));
            }
        }

        let (mv, score) = self.search_inner(
            board,
            0,
            depth,
            cancel_signal,
            false,
            Score::LossIn(0),
            Score::WinIn(0),
        )?;

        Ok((
            mv.and_then(|mv| board.apply_board_move(&mv).ok()),
            if board.active_color == COLOR_WHITE {
                score
            } else {
                -score
            },
        ))
    }

    pub fn stats(&self) -> EngineStats {
        self.stats
    }

    #[allow(clippy::too_many_arguments)]
    fn search_inner(
        &mut self,
        board: &Board,
        ply: u32,
        depth: u32,
        cancel_signal: &CancelSignal,
        allow_null: bool,
        alpha: Score,
        beta: Score,
    ) -> InterruptableResult<(Option<BoardMove>, Score)> {
        self.stats.nodes += 1;

        // Check the lock.
        if cancel_signal.is_stopped() {
            // Stop the thread.
            return Err(InterruptedError);
        }

        let alpha_orig = alpha;
        let mut alpha = alpha;
        let mut beta = beta;

        let tt_index = (board.zobrist_hash >> (64 - self.transposition_table_index_bits)) as usize;
        let tt_entry =
            self.transposition_table[tt_index].filter(|e| e.zobrist_hash == board.zobrist_hash);

        if let Some(entry) = tt_entry {
            if entry.depth >= depth {
                // The score was calculated to at least the depth we need.
                match entry.bound {
                    TranspositionTableBound::Exact => {
                        // We know the exact score. We can return it!
                        return Ok((entry.best_move, entry.score));
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
                    return Ok((entry.best_move, entry.score));
                }
            }
        }

        if depth == 0 {
            self.stats.leaves += 1;
            return Ok((None, self.quiescence(board, cancel_signal, alpha, beta)?));
        }

        // Null move pruning
        let null_move_depth_reduction = 2;
        if allow_null && ply > 0 && depth > null_move_depth_reduction + 1 && !board.is_check() {
            let null_move_board = board.apply_mutation(|_| {});
            let (_, null_move_score) = self.search_inner(
                &null_move_board,
                ply + 1,
                depth - null_move_depth_reduction - 1,
                cancel_signal,
                false,
                -beta,
                -alpha,
            )?;
            let null_move_score = -null_move_score;
            if null_move_score >= beta {
                // Null move pruning with verification
                return self.search_inner(
                    board,
                    ply,
                    depth - null_move_depth_reduction - 1,
                    cancel_signal,
                    false,
                    alpha,
                    beta,
                );
            }
        }

        let mut next_boards = board.next_boards();

        // Order moves: best move from transposition table goes first, then order heuristically.
        let tt_best_board = tt_entry
            .and_then(|e| e.best_move.map(|mv| board.apply_board_move(&mv).ok()))
            .flatten();
        next_boards.sort_by_cached_key(|b| {
            if let Some(tt_best_board) = &tt_best_board {
                if b == tt_best_board {
                    return -10_000;
                }
            }
            self.evaluator.evaluate_move_by_board(board, b)
        });

        if next_boards.is_empty() {
            let score = if board.is_check() {
                Score::LossIn(0)
            } else {
                Score::Value(0)
            };
            return Ok((None, score));
        }

        let mut best_board: Option<Board> = None;
        let mut best_score = Score::LossIn(0);

        for (index, b) in next_boards.into_iter().enumerate() {
            let is_first = index == 0;
            let (_, score) = if is_first {
                self.search_inner(&b, ply + 1, depth - 1, cancel_signal, true, -beta, -alpha)?
            } else {
                // Search with null window
                let (mv, score) = self.search_inner(
                    &b,
                    ply + 1,
                    depth - 1,
                    cancel_signal,
                    true,
                    (-alpha).add_value(-1),
                    -alpha,
                )?;
                if alpha < -score && -score < beta {
                    // Fail high
                    // Re-search
                    self.search_inner(&b, ply + 1, depth - 1, cancel_signal, true, -beta, -alpha)?
                } else {
                    (mv, score)
                }
            };
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

        // If our score in mate in X, add 1 to that
        if let Score::WinIn(n) = best_score {
            best_score = Score::WinIn(n + 1);
        }

        let best_move = best_board.and_then(|b| b.as_board_move(board));

        // Add to transposition table.
        let bound = if best_score <= alpha_orig {
            TranspositionTableBound::Upper
        } else if best_score >= beta {
            TranspositionTableBound::Lower
        } else {
            TranspositionTableBound::Exact
        };
        if self.transposition_table[tt_index]
            .map_or(true, |e| e.bound != TranspositionTableBound::Exact)
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

        Ok((best_move, best_score))
    }

    fn quiescence(
        &mut self,
        board: &Board,
        cancel_signal: &CancelSignal,
        alpha: Score,
        beta: Score,
    ) -> InterruptableResult<Score> {
        self.stats.quiescence_nodes += 1;

        let mut alpha = alpha;

        // Do a static eval. We do this to check that we don't make really dumb captures.
        // For example: if the only capture I can do is capture a defended pawn with our queen,
        // I am obviously not going to make that move in the game.
        let static_eval = self.evaluator.evaluate(board, board.active_color);
        alpha = alpha.max(static_eval);
        if static_eval >= beta {
            return Ok(beta);
        }

        // Check the lock.
        if cancel_signal.is_stopped() {
            // Stop the thread.
            return Err(InterruptedError);
        }

        // Keep captures only.
        let mut next_boards = board.next_boards();

        next_boards.retain(|b| {
            b.occupancy_bits_for(b.active_color) != board.occupancy_bits_for(b.active_color)
        });

        if next_boards.is_empty() {
            return Ok(static_eval);
        }

        next_boards.sort_by_cached_key(|b| self.evaluator.evaluate_move_by_board(board, b));

        for b in next_boards {
            let score = -self.quiescence(&b, cancel_signal, -beta, -alpha)?;
            alpha = alpha.max(score);
            if alpha >= beta {
                break;
            }
        }

        Ok(alpha)
    }
}

#[cfg(test)]
mod tests {
    use std::{
        env,
        thread::{self, sleep},
        time::Duration,
    };

    use crate::{
        bitwise_helper::BitwiseHelper,
        board::{Board, COLOR_BLACK, COLOR_WHITE},
        book::PolyglotOpeningBook,
        engine::EngineBuilder,
        evaluator::basic::BasicEvaluator,
        score::Score,
        threading::{CancelHandle, InterruptedError},
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
        assert_eq!(score, Score::WinIn(1));

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
        assert_eq!(score, Score::WinIn(2));

        // It got the right move.
        assert!(b.is_some());
        assert_eq!(
            b.unwrap().to_fen(),
            "r2qkb1r/pp2nppp/3p1N2/2p1N1B1/2BnP3/3P4/PPP2PPP/R2bK2R b KQkq - 2 1"
        );
    }

    #[test]
    fn perf_test_1() {
        // I think this only ever succeeded purely by accident.
        // It depends on being able to see a draw by insufficient material 24 ply away.
        // let board = Board::try_parse_fen("5r2/8/1R6/ppk3p1/2N3P1/P4b2/1K6/5B2 w - - 0 1").unwrap();

        // let mut engine = Engine::default();
        // let (b, _score) = engine.search(&board, 6);

        // // It got the right move.
        // assert_eq!(
        //     b.map(|b| b.to_fen()),
        //     Some(String::from("5r2/8/8/pRk3p1/2N3P1/P4b2/1K6/5B2 b - - 0 1")),
        // );
    }

    #[test]
    fn perf_test_2() {
        let board =
            Board::try_parse_fen("5rk1/p1nnqr1p/1p1p4/3Pp2Q/5p1N/1P4PB/P2R1P1P/4R1K1 w - - 0 1")
                .unwrap();

        let mut engine = Engine::default();
        let (b, _score) = engine.search(&board, 4);

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

        let (b, _score) = engine.search(&board, 8);

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

        let (b, _score) = engine.search(&board, 6);

        assert!(b.is_some());
        let b = b.unwrap();

        assert_eq!(b.as_move_string(&board), Some(String::from("Qxa4")));

        let board = b.apply_mutation(|b| {
            b.knights[COLOR_WHITE].move_bit(42, 32);
            b.queens[COLOR_BLACK] = 0;
        });

        let (b, _score) = engine.search(&board, 6);

        assert!(b.is_some());
        let b = b.unwrap();

        assert_eq!(b.as_move_string(&board), Some(String::from("Rd4")));
    }

    #[test]
    fn test_puzzle_3() {
        let board =
            Board::try_parse_fen("r5k1/4Qpp1/7p/r2pp3/4P3/3P1qP1/5P1P/R4RK1 w - - 0 1").unwrap();

        let mut engine = Engine::default();

        let (b, _score) = engine.search(&board, 6);
        assert!(b.is_some());
        let b = b.unwrap();

        assert_eq!(b.as_move_string(&board), Some(String::from("Rxa5")));

        let board =
            Board::try_parse_fen("6k1/4Qpp1/7p/r2pp3/4P3/3P1qP1/5P1P/5RK1 w - - 0 1").unwrap();

        let mut engine = Engine::default();

        let (b, _score) = engine.search(&board, 6);
        assert!(b.is_some());
        let b = b.unwrap();

        assert_eq!(b.as_move_string(&board), Some(String::from("Qd8+")));
    }

    #[test]
    fn test_puzzle_4() {
        // Mate in 3: why doesn't my engine see it?
        let board =
            Board::try_parse_fen("5R2/p2k1p1P/1P1P1PPr/bPpKBN1p/1pR3n1/7B/2P2N1P/1b6 w - - 0 1")
                .unwrap();

        let mut engine = Engine::default();

        let (b, score) = engine.search(&board, 8);
        assert!(b.is_some());
        let b = b.unwrap();

        println!("{}", score);

        assert_eq!(b.as_move_string(&board), Some(String::from("Nxh6")));
    }

    #[test]
    fn test_opening_book() {
        let opening_book_path = env::var("OPENING_BOOK");
        if opening_book_path.is_err() {
            return;
        }

        let board = Board::new_setup();

        let book =
            PolyglotOpeningBook::read(opening_book_path.expect("OPENING_BOOK should be set"))
                .expect("should be able to read opening book");
        let mut engine = EngineBuilder::new()
            .with_opening_book(Box::new(book))
            .build();

        let (b, _score) = engine.search(&board, 6);

        assert!(b.is_some());
        let b = b.unwrap();

        let mv = b.as_move_string(&board).unwrap();
        assert!(mv == "e4" || mv == "d4");
    }

    #[test]
    fn test_quiescence() {
        let mut engine = EngineBuilder::new()
            .with_evaluator(Box::new(BasicEvaluator::new()))
            .build();

        let cancel_handle = CancelHandle::default();

        // No captures possible in new setup.
        let board = Board::new_setup();
        let score = engine.quiescence(
            &board,
            &cancel_handle.signal(),
            Score::LossIn(0),
            Score::WinIn(0),
        );
        assert_eq!(score, Ok(Score::Value(0)));

        // No captures on empty board.
        let board = Board::new();
        let score = engine.quiescence(
            &board,
            &cancel_handle.signal(),
            Score::LossIn(0),
            Score::WinIn(0),
        );
        assert_eq!(score, Ok(Score::Value(0)));

        let board = Board::try_parse_fen("6k1/4qp1p/6p1/2b5/8/7P/2Q2PP1/2R3K1 w - - 0 1").unwrap();
        let score = engine.quiescence(
            &board,
            &cancel_handle.signal(),
            Score::LossIn(0),
            Score::WinIn(0),
        );
        assert_eq!(score, Ok(Score::Value(500)));

        let board = Board::try_parse_fen("6k1/4qp1p/6p1/2r5/8/7P/2Q2PP1/2B3K1 w - - 0 1").unwrap();
        let score = engine.quiescence(
            &board,
            &cancel_handle.signal(),
            Score::LossIn(0),
            Score::WinIn(0),
        );
        assert_eq!(score, Ok(Score::Value(-190)));

        let board = Board::try_parse_fen("6k1/4qp1p/6p1/2r5/8/7P/2Q2PP1/2B3K1 b - - 0 1").unwrap();
        let score = engine.quiescence(
            &board,
            &cancel_handle.signal(),
            Score::LossIn(0),
            Score::WinIn(0),
        );
        assert_eq!(score, Ok(Score::Value(1090)));

        let board = Board::try_parse_fen("6k1/4qp1p/6p1/2r5/8/7P/2Q2PP1/1B4K1 b - - 0 1").unwrap();
        let score = engine.quiescence(
            &board,
            &cancel_handle.signal(),
            Score::LossIn(0),
            Score::WinIn(0),
        );
        assert_eq!(score, Ok(Score::Value(590)));
    }

    #[test]
    fn threading() {
        let board = Board::try_parse_fen(
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 0",
        )
        .unwrap();

        let handles: Vec<_> = (0..6)
            .map(|_| {
                let board = board.clone();

                thread::spawn(move || {
                    let mut engine = Engine::default();
                    let (b, _score) = engine.search(&board, 4);
                    b
                })
            })
            .collect();

        for h in handles {
            let b = h.join().unwrap().unwrap();
            assert_eq!(b.as_move_string(&board), Some(String::from("dxe6")));
        }
    }

    #[test]
    fn cancelable_threading() {
        let board = Board::try_parse_fen(
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 0",
        )
        .unwrap();

        let cancel_handle = CancelHandle::new();
        let cancel_signal = cancel_handle.signal();

        thread::spawn(move || {
            sleep(Duration::from_millis(100));
            cancel_handle.stop();
        });

        let mut engine = Engine::default();
        let result = engine.cancelable_search(&board, 400, &cancel_signal);

        // The actual test here is whether this terminates with a reasonable value, considering depth = 400...
        assert_eq!(result, Err(InterruptedError));
    }
}
