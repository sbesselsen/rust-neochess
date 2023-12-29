use neochess::{bitboard::BitBoard, engine::Engine};
use std::io::{stdin, BufRead};

fn main() {
    let depth = 8;

    let mut engine = Engine::default();
    let mut board = BitBoard::new_setup();

    let input = read_option("Play as (w/b): ", vec!["w", "b"]);
    if input == "b" {
        let (b, _) = engine.minmax_cutoff(&board, depth);
        let b = b.expect("first move should always yield a board");
        println!(
            "My move: {}",
            board.move_as_string(&b).expect("first move must be valid")
        );
        board = b;
    }

    loop {
        let moves_boards: Vec<(String, BitBoard)> = board
            .next_boards()
            .into_iter()
            .map(|b| {
                (
                    board
                        .move_as_string(&b)
                        .expect("engine-generated boards should always be a valid move"),
                    b,
                )
            })
            .collect();
        if moves_boards.len() == 0 {
            println!("You are out of moves!");
            if board.is_checkmate() {
                println!("I win!");
            } else {
                println!("Draw...");
            }
            break;
        }
        let mv = read_option(
            "Your move: ",
            moves_boards.iter().map(|(m, _)| &m[..]).collect(),
        );
        let (_, player_move_board) = moves_boards
            .into_iter()
            .find(|(m, _)| m == &mv)
            .expect("read_option must have provided a valid option");

        let (engine_move_board, _) = engine.minmax_cutoff(&player_move_board, depth);
        if let Some(b) = engine_move_board {
            board = b;
            println!(
                "My move: {}",
                player_move_board
                    .move_as_string(&board)
                    .expect("player move must be valid")
            );
        } else {
            println!("I am out of moves!");
            if player_move_board.is_checkmate() {
                println!("You win!");
            } else {
                println!("Draw...");
            }
            break;
        }
    }
}

fn read_option(input: &str, options: Vec<&str>) -> String {
    println!("{}", input);
    let mut buf = String::new();
    loop {
        let result = stdin().lock().read_line(&mut buf);
        if let Ok(_) = result {
            let option = buf.trim_end();
            if options.contains(&option) {
                return option.to_string();
            }
        }
        buf.clear();
        println!("Invalid input");
    }
}
