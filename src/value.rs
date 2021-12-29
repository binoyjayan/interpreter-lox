
// Value is used to represent the result of evaluating expressions
// and executing statements. Although, it may look like `Literal`
// but can hold more information than that. Also we have a conversion
// of Literal types to value types using 'From' trait.

use std::fmt;
use rustyline::error::ReadlineError;

use crate::token::Literal;
use crate::interpreter::Interpreter;
use crate::function::{LoxCallable, NativeFunction, LoxFunction};


#[derive(Debug, Clone)]
pub enum Value {
    NativeFunction(NativeFunction),
    LoxFunction(LoxFunction),
    Str(String),
    Number(f64),
    Bool(bool),
    Nil,
}

#[derive(Debug)]
pub enum ErrorType {
    Str(String),
    ReturnFromFunction(Value),
    LoadError(std::io::Error),
    ReadlineError(ReadlineError),
}

// Implement custom error type for file read error
impl From<std::io::Error> for ErrorType {
    fn from(e: std::io::Error) -> Self {
        ErrorType::LoadError(e)
    }
}

// Implement custom error type for REPL readline error
impl From<ReadlineError> for ErrorType {
    fn from(e: ReadlineError) -> Self {
        ErrorType::ReadlineError(e)
    }
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ReturnFromFunction")
    }
}

impl From<Literal> for Value {
    fn from(literal: Literal) -> Self {
        match literal {
            Literal::Str(s) => Self::Str(s),
            Literal::Bool(b) => Self::Bool(b),
            Literal::Number(n) => Self::Number(n),
            Literal::Nil => Self::Nil,
            _ => {
                println!("No conversions available for literal {}", literal);
                Self::Nil
            }
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::NativeFunction(func) => write!(f, "{}", func),
            Self::LoxFunction(func) => write!(f, "{}", func),
            Self::Str(s) => write!(f, "{}", s),
            Self::Number(n) => write!(f, "{}", n),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Nil => write!(f, "nil"),
        }
    }
}

