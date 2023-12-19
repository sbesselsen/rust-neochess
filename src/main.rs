mod board;

use board::Board;

fn main() {
    let board = Board::new_setup();

    for b in board.next_boards() {
        println!("Board: {:?}", b);
    }
}
