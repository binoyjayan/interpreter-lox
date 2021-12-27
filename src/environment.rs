use anyhow::{anyhow, Result};
use std::collections::HashMap;

use crate::value::Value;
use crate::token::Token;

#[derive(Clone, Debug, Default)]
pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &Token) -> Result<Value> {
        if self.values.contains_key(&name.lexeme) {
            Ok(self.values.get(&name.lexeme).unwrap().clone())
        } else {
            Err(anyhow!(format!("Undefined variable '{}'.", name.lexeme)))
        }
    }

    pub fn put(&mut self, name: Token, value: Value) -> Result<()> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme, value);
            Ok(())
        } else {
            Err(anyhow!(format!("Undefined variable '{}'.", name.lexeme)))
        }
    }

}
