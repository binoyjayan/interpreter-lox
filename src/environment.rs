use std::result;
use std::collections::HashMap;

use crate::token::Token;
use crate::value::{Value, ErrorType};

#[derive(Clone, Debug, Default)]
pub struct Environment {
    // reference to the environment that encloses this environment
    // The top level environment will have a value of None.
    // To find the value of a variable, start from the inner most
    // block and walk up the chain until the global scope is reached.
    pub enclosing: Option<Box<Environment>>,
    pub values: HashMap<String, Value>,
}

impl Environment {

    // Create an environment with an enclosing environment
    pub fn new_from(enclosing: Self) -> Self {
        Self {
            enclosing: Some(Box::new(enclosing)),
            values: HashMap::default(),
        }
    }

    // Define a variable by creating an entry in the map
    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    // Get the value for an variable with the name 'name'
    pub fn get(&self, name: &Token) -> result::Result<Value, ErrorType> {
        if self.values.contains_key(&name.lexeme) {
            return Ok(self.values.get(&name.lexeme).unwrap().clone())
        }
        match &self.enclosing {
            Some(enclosing) => enclosing.get(name),
            None => Err(ErrorType::Str(format!("Undefined identifier '{}'.", name.lexeme))),
        }
    }

    // Modify the value of an variable expected to be available.
    pub fn put(&mut self, name: Token, value: Value) -> result::Result<Value, ErrorType> {
        // Check the environment in the current scope (inner most).
        // If present, assign value to that variable
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme, value);
            return Ok(Value::Nil)
        }
        match &mut self.enclosing {
            Some(enclosing) => enclosing.put(name, value),
            None => Err(ErrorType::Str(format!("Undefined identifier '{}'.", name.lexeme))),
        }
    }

    pub fn print(&self) {
        println!("BEGIN ENV:");
        for (key, val) in self.values.iter() {
            match val {
                Value::Number(n) => println!("env: {} = {}", key, n),
                _ => {},
            }
        }
        match &self.enclosing {
            Some(boxed_enclosing) => {
                println!("ENCLOSING ENV:");
                let enclosing = &*boxed_enclosing;
                for (key, val) in &enclosing.values {
                    match val {
                        Value::Number(n) => println!("env: {} = {}", key, n),
                        _ => { },
                    }
                }
            },
            _ => {
                println!("ENCLOSING ENV: None");
            }
        }
        println!("END ENV:");
    }
}
