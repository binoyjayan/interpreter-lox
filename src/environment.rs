use anyhow::{anyhow, Result};
use std::collections::HashMap;

use crate::value::Value;
use crate::token::Token;

#[derive(Clone, Debug, Default)]
pub struct Environment {
    // reference to the environment that encloses this environment
    // The top level environment will have a value of None.
    // To find the value of a variable, start from the inner most
    // block and walk up the chain until the global scope is reached.
    pub enclosing: Option<Box<Environment>>,
    values: HashMap<String, Value>,
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
    pub fn get(&self, name: &Token) -> Result<Value> {
        if self.values.contains_key(&name.lexeme) {
            return Ok(self.values.get(&name.lexeme).unwrap().clone())
        }
        match &self.enclosing {
            Some(enclosing) => enclosing.get(name),
            None => Err(anyhow!(format!("Undefined variable '{}'.", name.lexeme))),
        }
    }

    // Modify the value of an variable expected to be available.
    pub fn put(&mut self, name: Token, value: Value) -> Result<()> {
        // Check the environment in the current scope (inner most).
        // If present, assign value to that variable
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme, value);
            return Ok(())
        }
        match &mut self.enclosing {
            Some(enclosing) => enclosing.put(name, value),
            None => Err(anyhow!(format!("Undefined variable '{}'.", name.lexeme))),
        }
    }
}
