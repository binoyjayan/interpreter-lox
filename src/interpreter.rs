// The AST Tree-walk Interpreter
use anyhow::{anyhow,Result};

use crate::value::Value;
use crate::environment::Environment;
use crate::token::{Literal, Token, TokenType};
use crate::expr::{Expression, ExpressionVisitor};
use crate::stmt::{Statement, StatementVisitor};

pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: Environment::default(),
        }
    }

    pub fn interpret(&mut self, statements: Vec<Statement>) -> Result<()> {
        for statement in statements {
            // Catch runtime errors and display them
            if let Err(e) = self.execute(statement) {
                return Err(anyhow!(format!("Runtime: {}", e)));
            }
        }
        Ok(())
    }
}

impl Interpreter {
    // Being a dynamically typed language, perform implicit type conversions
    // for all types for the purposes of determining truthiness. false and
    // nil are falsey, and everything else is truthy
    fn is_truthy(value: &Value) -> bool {
        if let Value::Bool(b) = value {
            *b
        } else {
            !matches!(value, Value::Nil)
        }
    }

    // Check for equality
    fn is_equal(a: Value, b: Value) -> bool {
        match (a, b) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => (a - b).abs() < std::f64::EPSILON,
            _ => false,
        }
    }

    // To execute a block, create a new environment for the block’s
    // scope and pass it off to this other method
    fn execute_block(&mut self, statements: &[Statement]) -> Result<Value> {
        // The environment field in 'self' keeps changing when the scope changes.
        // It represents the current environment. That’s the environment that
        // corresponds to the innermost scope containing the code to be executed.
        self.environment = Environment::new_from(self.environment.clone());
        for statement in statements {
            self.execute(statement.clone())?;
        }
        // restores the previous value while exiting the scope or
        // if any of the statements in the block results in an error
        if let Some(enclosing) = self.environment.enclosing.clone() {
            self.environment = *enclosing;
        }
        Ok(Value::Nil)
    }
}

impl StatementVisitor<Value> for Interpreter {
    fn exec_if_stmt(&mut self, condition: Expression,
                    then_branch: Box<Statement>,
                    else_branch: Box<Option<Statement>>) -> Result<Value> {
        // If truthy, it executes the then branch. Otherwise,
        // if there is an else branch, then execute that.
        if Self::is_truthy(&self.evaluate(condition)?) {
            self.execute(*then_branch)?;
        } else if let Some(else_branch) = *else_branch {
            self.execute(else_branch)?;
        }
        Ok(Value::Nil)
    }

    fn exec_block(&mut self, statements: Vec<Statement>) -> Result<Value> {
        self.execute_block(&statements)
    }

    fn exec_expr(&mut self, statement: Expression) -> Result<Value> {
        self.evaluate(statement)
    }

    fn exec_print(&mut self, statement: Expression) -> Result<Value> {
        let value = self.evaluate(statement)?;
        println!("{}", value);
        Ok(value)
    }

    // variable declaration
    fn exec_var(&mut self, name: Token, initializer: Option<Expression>) -> Result<Value> {
        // If the variable has an initializer, we evaluate it and assign the result
        // to it, otherwise assign 'Nil' to it.
        let value = initializer.map_or_else(|| -> Result<Value> { Ok(Value::Nil)}, |initializer| -> Result<Value> {
            self.evaluate(initializer)
        });
        self.environment.define(name.lexeme, value?);
        Ok(Value::Nil)
    }

    fn exec_while(&mut self, condition: Expression, body: Box<Statement>) -> Result<Value> {
        while Self::is_truthy(&self.evaluate(condition.clone())?) {
            self.execute(*body.clone())?;
        }
        Ok(Value::Nil)
    }
}

impl ExpressionVisitor<Value> for Interpreter {
    fn eval_assign(&mut self, name: Token, value: Box<Expression>) -> Result<Value> {
        let value = self.evaluate(*value)?;
        self.environment.put(name, value.clone())?;
        Ok(value.into())
    }

    // Evaluate left and right subexpressions first and then perform arithmetic,
    // logical or equality operations. The arithmetic operation produces result
    // whose type is same as  the operands. However, the logical and equality
    // operators produce a boolean result.
    fn eval_binary(&mut self, left: Box<Expression>, operator: Token, right: Box<Expression>) -> Result<Value> {
        let left = self.evaluate(*left)?;
        let right = self.evaluate(*right)?;
        match operator.ttype {
            TokenType::Minus => {
                if let (Value::Number(l), Value::Number(r)) = (&left, &right) {
                    Ok(Value::Number(l - r))
                } else {
                    println!("Subtraction not allowed on '{}' and '{}'", left, right);
                    Ok(Value::Nil)
                }
            },
            TokenType::Slash => {
                if let (Value::Number(l), Value::Number(r)) = (left.clone(), right.clone()) {
                    Ok(Value::Number(l / r))
                } else {
                    println!("Division not allowed on '{}' and '{}'", left, right);
                    Ok(Value::Nil)
                }
            },
            TokenType::Star => {
                if let (Value::Number(l), Value::Number(r)) = (left.clone(), right.clone()) {
                    Ok(Value::Number(l * r))
                } else {
                    println!("Multiplication not allowed on '{}' and '{}'", left, right);
                    Ok(Value::Nil)
                }
            },
            TokenType::Plus => {
                // Plus is special as it can handle both numberic as well as strings
                if let (Value::Number(l), Value::Number(r)) = (left.clone(), right.clone()) {
                    Ok(Value::Number(l + r))
                } else if let (Value::Str(l), Value::Str(r)) = (left.clone(), right.clone()) {
                    Ok(Value::Str(l + &r))
                } else {
                    println!("Addition not allowed on '{}' and '{}'", left, right);
                    Ok(Value::Nil)
                }
            },
            TokenType::Greater => {
                if let (Value::Number(l), Value::Number(r)) = (left.clone(), right.clone()) {
                    Ok(Value::Bool(l > r))
                } else {
                    println!("Relational operation not allowed on '{}' and '{}'", left, right);
                    Ok(Value::Nil)
                }
            },
            TokenType::GreaterEqual => {
                if let (Value::Number(l), Value::Number(r)) = (left.clone(), right.clone()) {
                    Ok(Value::Bool(l >= r))
                } else {
                    println!("Relational operation not allowed on '{}' and '{}'", left, right);
                    Ok(Value::Nil)
                }
            },
            TokenType::Less => {
                if let (Value::Number(l), Value::Number(r)) = (left.clone(), right.clone()) {
                    Ok(Value::Bool(l < r))
                } else {
                    println!("Relational operation not allowed on '{}' and '{}'", left, right);
                    Ok(Value::Nil)
                }
            },
            TokenType::LessEqual => {
                if let (Value::Number(l), Value::Number(r)) = (left.clone(), right.clone()) {
                    Ok(Value::Bool(l <= r))
                } else {
                    println!("Relational operation not allowed on '{}' and '{}'", left, right);
                    Ok(Value::Nil)
                }
            },
            TokenType::BangEqual => Ok(Value::Bool(!Self::is_equal(left, right))),
            TokenType::EqualEqual => Ok(Value::Bool(Self::is_equal(left, right))),
            _ => Ok(Value::Nil),
        }
    }

    // Send the grouped expression back into the interpreter’s visitor implementation
    fn eval_grouping(&mut self, expr: Box<Expression>) -> Result<Value> {
        self.evaluate(*expr)
    }

    // Simplest all expression. Just convert the literal to a 'Value'
    // Do not call this when an identifier is encountered.
    fn eval_literal(&mut self, literal: Literal) -> Result<Value> {
        Ok(literal.into())
    }

    // Here, the left operand is evaluated first.Look at its value to see if it
    // forms a short-circuit. If not, and only then, the right operand is evaluated.
    fn eval_logical(&mut self, left: Box<Expression>, operator: Token, right: Box<Expression>) -> Result<Value> {
        let left = self.evaluate(*left)?;
        if operator.ttype == TokenType::Or {
            if Self::is_truthy(&left) {
                // if lhs of an 'or' expresssion is true, not need to evaluate 'rhs'
                return Ok(left);
            }
        } else {
            // if lhs of an 'and' expresssion is false, not need to evaluate 'rhs'
            if !Self::is_truthy(&left) {
                return Ok(left);
            }
        }
        return Ok(self.evaluate(*right)?);
    }

    // unary expressions have a single subexpression must be evaluated first
    // Then apply the unary operator itself to the result. Here, the minus ('-')
    // operator negates the subexpression, whereas the Bang ('!') operator 
    // inverts the truth value.
    fn eval_unary(&mut self, operator: Token, value: Box<Expression>) -> Result<Value> {
        let value = self.evaluate(*value)?;
        match operator.ttype {
            TokenType::Minus => {
                if let Value::Number(n) = value {
                    Ok(Value::Number(-n))
                } else {
                    println!("Negation operation is not allowed on '{}'", value);
                    Ok(Value::Nil)
                }
            },
            TokenType::Bang => Ok(Value::Bool(!Self::is_truthy(&value))),
            _ => Ok(Value::Nil),
        }
    }

    // Evaluate a variable expression. Produce a runtime error
    // when an undefined variable is used.
    fn eval_variable(&self, expr: Token) -> Result<Value> {
        self.environment.get(&expr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn ast_test1() -> Result<()>  {
        //-12.34 + (56.78)   --->> 44.44
        let binary: Expression = Expression::Binary(Box::new(Expression::Unary(Token {ttype: TokenType::Minus, lexeme: "-".into(), literal:None, line: 1, col: 1},
                                                                               Box::new(Expression::LiteralExpression(Literal::Number(12.34))))),
                                                    Token {ttype: TokenType::Plus, lexeme: "+".into(), literal: None, line: 1, col: 6},
                                                    Box::new(Expression::Grouping(Box::new(Expression::LiteralExpression(Literal::Number(56.78))))));
        Interpreter::new().interpret(vec![Statement::Expression(binary)])?;
        // assert_eq!(format!("{}", result), format!("{}", Value::Number(44.44)));
        Ok(())
    }

    #[test]
    fn ast_test2() -> Result<()>  {
        //-11.22 == (-11.22)  --->> true
        let binary: Expression = Expression::Binary(Box::new(Expression::Unary(Token {ttype: TokenType::Minus, lexeme: "-".into(), literal:None, line: 2, col: 1},
                                                                               Box::new(Expression::LiteralExpression(Literal::Number(11.22))))),
                                                    Token {ttype: TokenType::EqualEqual, lexeme: "==".into(), literal: None, line: 2, col: 8},
                                                    Box::new(Expression::Grouping(Box::new(Expression::LiteralExpression(Literal::Number(-11.22))))));
        Interpreter::new().interpret(vec![Statement::Expression(binary)])?;
        // assert_eq!(format!("{}", result), format!("{}", Value::Bool(true)));
        Ok(())
    }
}
