use std::collections::BinaryHeap;

use crate::{
    bitboard::{BitBoard, COLOR_WHITE},
    evaluator::{DefaultEvaluator, Evaluator, EvaluatorScore},
};

#[derive(Debug)]
pub struct Engine<E> {
    evaluator: E,
    frames: Vec<EngineFrame>,
    queue: BinaryHeap<QueueItem>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct EngineFrame {
    board: BitBoard,
    score: EvaluatorScore,
    parent: Option<usize>,
    descendants: Option<Vec<usize>>,
    depth: u32,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct QueueItem {
    index: usize,
    priority: u32,
}

impl PartialOrd for QueueItem {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for QueueItem {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.priority.cmp(&self.priority)
    }
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
        Engine {
            evaluator,
            frames: Default::default(),
            queue: Default::default(),
        }
    }
}

impl Engine<DefaultEvaluator> {
    pub fn new() -> Self {
        Engine {
            evaluator: Default::default(),
            frames: Default::default(),
            queue: Default::default(),
        }
    }

    pub fn start_from_board(&mut self, board: BitBoard) {
        // TODO: reuse if we already know this board
        let score = self.evaluator.evaluate(&board);
        self.frames.clear();
        self.frames.push(EngineFrame {
            board,
            score,
            parent: None,
            descendants: None,
            depth: 0,
        });
        self.queue.push(QueueItem {
            index: 0,
            priority: 0,
        });
    }

    pub fn iterate(&mut self) {
        if let Some(QueueItem { index, .. }) = self.queue.pop() {
            self.iterate_index(index);
        }
    }

    fn iterate_index(&mut self, index: usize) {
        let EngineFrame {
            board,
            descendants,
            depth,
            ..
        } = &self.frames[index];

        if descendants.is_some() {
            return;
        }

        // Compute the next boards and add their frames.
        let next_boards = board.next_boards();
        let depth = *depth + 1;

        let mut scores: Vec<EvaluatorScore> = vec![];
        let mut descendant_indices: Vec<usize> = vec![];

        for b in next_boards {
            let score = self.evaluator.evaluate(&b);

            let descendant_index = self.frames.len();
            descendant_indices.push(descendant_index);

            self.frames.push(EngineFrame {
                board: b,
                score,
                parent: Some(index),
                descendants: None,
                depth,
            });
            if let EvaluatorScore::Value(_) = &score {
                // Only go deeper if the game has not ended (no infinite score).
                self.queue.push(QueueItem {
                    index: descendant_index,
                    priority: depth,
                });
            }
            scores.push(score);
        }

        self.frames[index].descendants = Some(descendant_indices);

        // Do a single upward propagation pass.
        self.propagate_upward_with_scores(index, scores);
    }

    fn propagate_upward(&mut self, index: usize) {
        let EngineFrame { descendants, .. } = &self.frames[index];
        if let Some(indices) = descendants {
            let scores: Vec<EvaluatorScore> = indices
                .iter()
                .map(|&idx| self.frames[idx].score.clone())
                .collect();
            self.propagate_upward_with_scores(index, scores);
        }
    }

    fn propagate_upward_with_scores(&mut self, index: usize, scores: Vec<EvaluatorScore>) {
        let EngineFrame { board, parent, .. } = &self.frames[index];
        let score = *if board.active_color == COLOR_WHITE {
            // The aggregate score is the max score of all the following boards.
            scores.iter().max()
        } else {
            scores.iter().min()
        }
        .expect("scores should contain at least 1 element");
        let parent = parent.clone();
        self.frames[index].score = score;
        if let Some(parent_index) = parent {
            self.propagate_upward(parent_index);
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

        let mut engine = Engine::new();
        engine.start_from_board(board);
        for _ in 0..800_000 {
            engine.iterate();
        }

        println!("{}", engine.frames.len());
        println!("{}", engine.frames.last().unwrap().depth);

        assert_eq!(engine.frames[0].score, EvaluatorScore::PlusInfinity);
    }
}
