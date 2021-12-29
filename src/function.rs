use std::{fmt, result};
use crate::token::Token;
use crate::stmt::Statement;
use crate::environment::Environment;
use crate::value::{Value, ErrorType};
use crate::interpreter::Interpreter;

// arity is the number of arguments a function or operation expects
pub const MAX_ARITY: usize = 255;

pub trait LoxCallable {
    fn arity(&self, interpreter: &Interpreter) -> usize;
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> result::Result<Value, ErrorType>;
}

// These are functions that the interpreter exposes to user code
// but that are implemented in the host language / operating system
// The ideal way of doing this would be through a mechanism known as
// Foreign Function Interface (FFI)
#[derive(Clone)]
pub struct NativeFunction {
    pub name: String,
    pub arity: usize,
    pub callable: fn(&mut Interpreter, &[Value]) -> result::Result<Value, ErrorType>,
}

impl LoxCallable for NativeFunction {
    fn arity(&self, _interpreter: &Interpreter) -> usize {
        self.arity
    }
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> result::Result<Value, ErrorType> {
        (self.callable)(interpreter, args)
    }
}

impl fmt::Display for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "native-fn <{}>", self.name)
    }
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "native-fn <{:?}>", self.name)
    }
}

#[derive(Clone, Debug)]
pub struct LoxFunction {
    pub name: Token,
    pub parameters: Vec<Token>,
    pub body: Statement,
}

impl LoxFunction {
    pub fn new(name: Token, parameters: Vec<Token>, body: Statement) -> Self {
        Self {
            name,
            parameters,
            body,
        }
    }
}

impl LoxCallable for LoxFunction {
    fn arity(&self, _interpreter: &Interpreter) -> usize {
        self.parameters.len()
    }
    // Parameters are core to functions, especially the fact that a function
    // encapsulates its parameters—no other code outside of the function can see them.
    // This means each function gets its own environment where it stores those variables.
    // Further, this environment must be created dynamically. Each function call gets
    // its own environment. Otherwise, recursion would break. If there are multiple calls
    // to the same function in play at the same time, each needs its own environment, even
    // though they are all calls to the same function.
    fn call(&self, interpreter: &mut Interpreter, arguments: &[Value]) -> result::Result<Value, ErrorType> {
        if let Statement::Block(statements) = &self.body {
            let mut env = interpreter.environment.clone();
            for (i, arg) in arguments.iter().enumerate() {
                env.define(self.parameters[i].lexeme.clone(), arg.clone())
            }
            match interpreter.execute_block(&statements, env) {
                Ok(v) => {
                    return Ok(v);
                },
                Err(e) => {
                    match e {
                        ErrorType::ReturnFromFunction(val) => {
                            return Ok(val)
                        },
                        _ => return Err(e),
                    }
                }
            }
        }
        Ok(Value::Nil)
    }
}

impl fmt::Display for LoxFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "fn <{}>", self.name.lexeme)
    }
}
