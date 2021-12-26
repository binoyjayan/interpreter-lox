
// Value is used to represent the result of evaluating expressions
// and executing statements. Although, it may look like `Literal`
// but can hold more information than that. Also we have a conversion
// of Literal types to value types using 'From' trait.

use std::fmt;
use crate::token::Literal;

#[derive(Debug, Clone)]
pub enum Value {
    Identifier(String),
    Str(String),
    Number(f64),
    Bool(bool),
    Nil,
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
            Self::Identifier(s) => write!(f, "{}", s),
            Self::Str(s) => write!(f, "{}", s),
            Self::Number(n) => write!(f, "{}", n),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Nil => write!(f, "nil"),
        }
    }
}
