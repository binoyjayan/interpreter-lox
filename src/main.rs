use std::{env};

mod scanner;
mod token;

use scanner::Scanner;

// struct Lox {
//     had_error: bool,
// }

// pub fn error(line: i32, message: String) {
//     println!("{} {}", line, message);
// }

// fn report(line: i32, wh: String, message: String) {
//     println!("[line {} Error {}: {} ", line, wh, message);
// }

//static had_error:bool = false;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() == 2 {
        run_file(&args[1]);
    } else if args.len() == 1 {
        run_prompt();
    } else {
        println!("Usage: {} <lox-script>", args[0]);
        std::process::exit(1);
    }
}

pub fn run_prompt() {
    loop {
        let mut rl = rustyline::Editor::<()>::new();
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => run(line),
            Err(_) => {
                println!("Exit");
                break;
            }
        }
    }
}

pub fn run_file(filename: &str) {
    println!("file: {}", filename);
    match std::fs::read_to_string(filename) {
        Ok(v) => run(v),
        Err(e) => return {
            println!("Failed to read file {} - {}", filename, e)
        },
    }
}

pub fn run(source: String) {
    println!("Source: {}", source);
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();

    for token in tokens {
        println!("{:?}", token);
    }

}

