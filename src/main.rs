use std::{env};
use std::sync::atomic;
use anyhow::Result;

mod expr;
mod stmt;
mod token;
mod value;
mod parser;
mod scanner;
mod environment;
mod interpreter;
mod ast_printer;

use parser::Parser;
use scanner::Scanner;
use token::{Token, TokenType};

use crate::interpreter::Interpreter;

static HAD_ERROR: atomic::AtomicBool = atomic::AtomicBool::new(false);

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() == 2 {
        run_file(&args[1])?;
    } else if args.len() == 1 {
        run_prompt()?;
    } else {
        println!("Usage: {} <lox-script>", args[0]);
        std::process::exit(1);
    }
    Ok(())
}

pub fn run_prompt() -> Result<()> {
    loop {
        let mut rl = rustyline::Editor::<()>::new();
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => run(line)?,
            Err(_) => {
                println!("Exit");
                break;
            }
        }
    }
    Ok(())
}

pub fn run_file(filename: &str) -> Result<()> {
    let s = std::fs::read_to_string(filename)?;
    run(s)
}
pub fn print_tokens(tokens: &Vec<Token>) {
    for token in tokens {
        println!("{:?}", token);
    }
}

pub fn run(source: String) -> Result<()> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();
    // print_tokens(&tokens);
    let statements = Parser::new(tokens).parse();
    Interpreter::new().interpret(statements)
}

pub fn error(line: usize, col: usize, message: &str) {
    report(line, col, "", message);
}

pub fn error_at_token(token: &Token, message: &str) {
    if token.ttype == TokenType::Eof {
        report(token.line, token.col, " at end", message);
    } else {
        report(token.line, token.col, &format!(" at '{}'", token.lexeme), message);
    }
}

fn report(line: usize, col:usize, lexeme: &str, message: &str) {
    println!("[line {} col {}] Error{}: {}", line, col, lexeme, message);
    set_had_error(true);
}

fn had_error() -> bool {
    HAD_ERROR.load(atomic::Ordering::Relaxed)
}

fn set_had_error(b: bool) {
    HAD_ERROR.store(b, atomic::Ordering::Relaxed)
}
