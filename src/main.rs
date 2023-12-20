mod bitwise_helper;
mod board;

use board::Board;

fn main() {
    let board = Board::new_setup();

    println!("Initial board: {:?}", board);

    for b in board.next_boards() {
        println!("{:?}", b);
    }
}
