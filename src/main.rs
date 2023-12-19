mod board;

use board::Board;

fn main() {
    let board = Board::new_setup();
    println!("Board: {:?}", board);
}
