use board::Board;

mod bitwise_helper;
mod board;

fn main() {
    let board = Board::new_setup();

    println!("Initial board: {:?}", board);

    for b in board.next_boards() {
        println!("{:?}", b);
    }
}
