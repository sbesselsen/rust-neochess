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
    children: Option<Vec<usize>>,
    depth: u32,
    dropped: bool,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct QueueItem {
    index: usize,
    priority: i32,
}

impl PartialOrd for QueueItem {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for QueueItem {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.priority.cmp(&other.priority)
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
            children: None,
            depth: 0,
            dropped: false,
        });
        self.queue.push(QueueItem {
            index: 0,
            priority: 0,
        });
    }

    pub fn iterate(&mut self) -> bool {
        if let Some(QueueItem { index, .. }) = self.queue.pop() {
            self.iterate_index(index);
            true
        } else {
            false
        }
    }

    fn iterate_index(&mut self, index: usize) {
        let EngineFrame {
            board,
            children: descendants,
            depth,
            dropped,
            ..
        } = &self.frames[index];

        if *dropped {
            // No longer relevant for some reason.
            return;
        }

        if descendants.is_some() {
            // Already computed!
            return;
        }

        // Compute the next boards and add their frames.
        let next_boards = board.next_boards();
        if next_boards.is_empty() {
            // TODO: handle stalemate properly
            // TODO: and ridiculous smothered mates like 8/8/2K5/8/8/6N1/pppppppp/rrrrrrrk b - - 0 1
            // Nothing to do here.
            return;
        }
        let depth = *depth + 1;

        let scores: Vec<EvaluatorScore> = next_boards
            .iter()
            .map(|b| self.evaluator.evaluate(b))
            .collect();
        let mut descendant_indices: Vec<usize> = vec![];

        for (i, b) in next_boards.into_iter().enumerate() {
            let score = scores[i];

            // TODO: perhaps deduplicate if we have already seen this board?
            // But that is tricky, because once we allow multiple ancestors,
            // we could get loops. How would that even work?

            let descendant_index = self.frames.len();
            descendant_indices.push(descendant_index);

            self.frames.push(EngineFrame {
                board: b,
                score,
                parent: Some(index),
                children: None,
                depth,
                dropped: false,
            });
            if let EvaluatorScore::Value(_) = &score {
                // Only go deeper if the game has not ended (no infinite score).
                self.queue.push(QueueItem {
                    index: descendant_index,
                    priority: -(depth as i32),
                });
            }
        }

        self.frames[index].children = Some(descendant_indices);

        // Do a single upward propagation pass.
        self.propagate_upward_with_scores(index, &scores);
    }

    fn max_score(scores: &[EvaluatorScore], active_color: usize) -> Option<&EvaluatorScore> {
        if active_color == COLOR_WHITE {
            // The aggregate score is the max score of all the following boards.
            scores.iter().max()
        } else {
            scores.iter().min()
        }
    }

    fn propagate_upward(&mut self, index: usize) {
        let EngineFrame {
            children: descendants,
            ..
        } = &self.frames[index];
        if let Some(indices) = descendants {
            let scores: Vec<EvaluatorScore> =
                indices.iter().map(|&idx| self.frames[idx].score).collect();
            self.propagate_upward_with_scores(index, &scores);
        }
    }

    fn propagate_upward_with_scores(&mut self, index: usize, scores: &[EvaluatorScore]) {
        let EngineFrame { board, parent, .. } = &self.frames[index];
        let active_color = board.active_color;
        let score = *Self::max_score(scores, board.active_color).expect(
            "propagate_upward_with_scores() should only be called if there is at least 1 score",
        );
        let parent = *parent;
        self.frames[index].score = score;

        if score.is_win(active_color) {
            self.drop_descendants(index);
        }

        if let Some(parent_index) = parent {
            self.propagate_upward(parent_index);
        }
    }

    fn drop_descendants(&mut self, index: usize) {
        if index == 0 {
            // Special case.
            for frame in self.frames.iter_mut() {
                frame.dropped = true;
            }
            self.queue.clear();
            return;
        }
        let mut undropped_descendant_indices: Vec<usize> = vec![];
        fn walk(output: &mut Vec<usize>, frames: &Vec<EngineFrame>, idx: usize) {
            let frame = &frames[idx];
            if frame.dropped {
                // Already dropped.
                return;
            }
            if let Some(children) = &frame.children {
                output.extend(children);
                for child_idx in children {
                    walk(output, frames, *child_idx);
                }
            }
        }

        walk(&mut undropped_descendant_indices, &self.frames, index);

        for idx in undropped_descendant_indices {
            self.frames[idx].dropped = true;
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
        for _ in 0..80_000 {
            if !engine.iterate() {
                break;
            }
        }

        // The engine notices this is checkmate.
        assert_eq!(engine.frames[0].score, EvaluatorScore::PlusInfinity);

        // The engine didn't go more than halfmoves deep.
        let max_depth = engine.frames.iter().map(|f| f.depth).max();
        assert_eq!(max_depth, Some(3));
    }

    #[test]
    #[ignore]
    fn mate_in_2() {
        // Taken from https://wtharvey.com/m8n2.txt
        let board = BitBoard::try_parse_fen(
            "r2qkb1r/pp2nppp/3p4/2pNN1B1/2BnP3/3P4/PPP2PPP/R2bK2R w KQkq - 1 0",
        )
        .unwrap();

        let mut engine = Engine::new();
        engine.start_from_board(board);

        // TODO: while it works, this takes *way* too many steps and takes ~8GiB of memory!
        for _ in 0..2_000_000 {
            if !engine.iterate() {
                break;
            }
        }

        // The engine notices this is checkmate.
        assert_eq!(engine.frames[0].score, EvaluatorScore::PlusInfinity);
    }
}
