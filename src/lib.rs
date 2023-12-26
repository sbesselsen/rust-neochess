pub mod bitboard;
mod bitwise_helper;
pub mod engine;
pub mod evaluator;
mod zobrist_constants {
    include!(concat!(env!("OUT_DIR"), "/zobrist_constants.rs"));
}
