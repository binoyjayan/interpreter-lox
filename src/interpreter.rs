// The AST Tree-walk Interpreter
use std::result;
use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::environment::Environment;
use crate::value::{Value, ErrorType};
use crate::token::{Literal, Token, TokenType};
use crate::expr::{Expression, ExpressionVisitor};
use crate::stmt::{Statement, StatementVisitor};
use crate::function::{LoxCallable, NativeFunction, LoxFunction};

pub struct Interpreter {
    pub environment: Environment,
    globals: Environment,
}

impl Interpreter {
    fn create_globals() -> Environment {
        let mut globals_venv = HashMap::new();
        globals_venv.insert(
            String::from("clock"),
            Value::NativeFunction(NativeFunction {
                    name: String::from("clock"),
                    arity: 0,
                    callable: |_, _| {
                        let start = SystemTime::now();
                        let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap();
                        Ok(Value::Number(since_the_epoch.as_millis() as f64))
                    },
                },
            ),
        );
        Environment {
            enclosing: None,
            values: globals_venv,
        }
    }
    pub fn new() -> Self {
        Self {
            environment: Environment::default(),
            globals: Self::create_globals(),
        }
    }

    pub fn interpret(&mut self, statements: Vec<Statement>) -> result::Result<Value, ErrorType> {
        for statement in statements {
            // Catch runtime errors and display them
            if let Err(e) = self.execute(statement) {
                return Err(ErrorType::Str(format!("Runtime: {}", e)));
            }
        }
        Ok(Value::Nil)
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

    fn lookup(&self, sym: &Token) -> result::Result<Value, ErrorType> {
        match self.environment.get(sym) {
            Ok(val) => Ok(val),
            Err(_) => self.globals.get(sym),
        }
    }

    fn block_restore_environment(&mut self) {
        if let Some(enclosing) = self.environment.enclosing.clone() {
            self.environment = *enclosing;
        }
    }

    // To execute a block, create a new environment for the block’s
    // scope and pass it off to this other method
    pub fn execute_block(&mut self, statements: &[Statement], new_environment: Environment) -> result::Result<Value, ErrorType> {
        // The environment field in 'self' keeps changing when the scope changes.
        // It represents the current environment. That’s the environment that
        // corresponds to the innermost scope containing the code to be executed.
        self.environment = new_environment;
        println!("BJ: creating new env...");
        self.environment.print();
        for stmt in statements {
            let statement = stmt.clone();
            // if the statement is a return statement, then return immediately
            // otherwise execute all statements
            match statement {
                Statement::Return(ret_expr) => {
                    let value = self.execute(Statement::Return(ret_expr))?;
                    return Err(ErrorType::ReturnFromFunction(value));
                }
                _ => {
                    // return immediately if encountered 'ReturnFromFunction'
                    match self.execute(statement) {
                        Err(e) => {
                            println!("BJ: Restoring env 1...");
                            self.block_restore_environment();
                            self.environment.print();
                            return Err(e);
                        }
                        _ => {}
                    }
                }
            }
        }
        // restores the previous value while exiting the scope or
        // if any of the statements in the block results in an error
        println!("BJ: Restoring env 2...");
        self.block_restore_environment();
        self.environment.print();
        // if let Some(enclosing) = self.environment.enclosing.clone() {
        //     self.environment = *enclosing;
        // }
        Ok(Value::Nil)
    }
}

impl StatementVisitor<Value, ErrorType> for Interpreter {
    fn exec_if_stmt(&mut self, condition: Expression,
                    then_branch: Box<Statement>,
                    else_branch: Box<Option<Statement>>) -> result::Result<Value, ErrorType> {
        // If truthy, it executes the then branch. Otherwise,
        // if there is an else branch, then execute that.
        if Self::is_truthy(&self.evaluate(condition)?) {
            self.execute(*then_branch)?;
        } else if let Some(else_branch) = *else_branch {
            self.execute(else_branch)?;
        }
        Ok(Value::Nil)
    }

    fn exec_block(&mut self, statements: Vec<Statement>) -> result::Result<Value, ErrorType> {
        let new_env = Environment::new_from(self.environment.clone());
        self.execute_block(&statements, new_env)
    }

    fn exec_expr(&mut self, statement: Expression) -> result::Result<Value, ErrorType> {
        self.evaluate(statement)
    }

    fn exec_print(&mut self, statement: Expression) -> result::Result<Value, ErrorType> {
        let value = self.evaluate(statement)?;
        println!("{}", value);
        Ok(value)
    }

    fn exec_return(&mut self, retval: Option<Expression>) -> result::Result<Value, ErrorType> {
        match retval {
            Some(statement) => {
                // evaluate optional expression passed to return statement
                let value = self.evaluate(statement)?;
                Ok(value)
            }
            None => Ok(Value::Nil)
        }
    }

    // variable declaration
    fn exec_var(&mut self, name: Token, initializer: Option<Expression>) -> result::Result<Value, ErrorType> {
        // If the variable has an initializer, we evaluate it and assign the result
        // to it, otherwise assign 'Nil' to it.
        let value = initializer.map_or_else(|| -> result::Result<Value, ErrorType> { Ok(Value::Nil)}, |initializer| -> result::Result<Value, ErrorType> {
            self.evaluate(initializer)
        });
        self.environment.define(name.lexeme, value?);
        Ok(Value::Nil)
    }

    fn exec_fun(&mut self, name: Token, params: Vec<Token>, body: Box<Statement>) -> result::Result<Value, ErrorType> {
        let fun_name = name.lexeme.clone();
        let function: LoxFunction = LoxFunction::new(name, params, *body);
        self.environment.define(fun_name.clone(), Value::LoxFunction(function));
        Ok(Value::Nil)
    }

    fn exec_while(&mut self, condition: Expression, body: Box<Statement>) -> result::Result<Value, ErrorType> {
        while Self::is_truthy(&self.evaluate(condition.clone())?) {
            self.execute(*body.clone())?;
        }
        Ok(Value::Nil)
    }
}

impl ExpressionVisitor<Value, ErrorType> for Interpreter {
    fn eval_assign(&mut self, name: Token, value: Box<Expression>) -> result::Result<Value, ErrorType> {
        let value = self.evaluate(*value)?;
        self.environment.put(name, value.clone())?;
        Ok(value.into())
    }

    // Evaluate left and right subexpressions first and then perform arithmetic,
    // logical or equality operations. The arithmetic operation produces result
    // whose type is same as  the operands. However, the logical and equality
    // operators produce a boolean result.
    fn eval_binary(&mut self, left: Box<Expression>, operator: Token, right: Box<Expression>) -> result::Result<Value, ErrorType> {
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
    fn eval_grouping(&mut self, expr: Box<Expression>) -> result::Result<Value, ErrorType> {
        self.evaluate(*expr)
    }

    // Simplest all expression. Just convert the literal to a 'Value'
    // Do not call this when an identifier is encountered.
    fn eval_literal(&mut self, literal: Literal) -> result::Result<Value, ErrorType> {
        Ok(literal.into())
    }

    // Here, the left operand is evaluated first.Look at its value to see if it
    // forms a short-circuit. If not, and only then, the right operand is evaluated.
    fn eval_logical(&mut self, left: Box<Expression>, operator: Token, right: Box<Expression>) -> result::Result<Value, ErrorType> {
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
    fn eval_unary(&mut self, operator: Token, value: Box<Expression>) -> result::Result<Value, ErrorType> {
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

    fn eval_call(&mut self, callee: Box<Expression>, args: Vec<Expression>) -> result::Result<Value, ErrorType> {
        // First, evaluate the expression for the callee. Typically, this 
        // expression is just an identifier that looks up the function
        // by its name, but it could be anything.
        let callee = self.evaluate(*callee)?;

        // evaluate the function arguments first
        let mut arguments = Vec::new();
        for arg in args {
            arguments.push(self.evaluate(arg)?);
        }

        match as_callable(&callee) {
            Some(function) => {
                function.call(self, &arguments)
            }
            None => {
                Err(ErrorType::Str(format!("value {:?} is not callable", callee)))
            }
        }
    }

    // Evaluate a variable expression. Produce a runtime error
    // when an undefined variable is used.
    fn eval_variable(&self, expr: Token) -> result::Result<Value, ErrorType> {
        // self.environment.get(&expr)
        self.lookup(&expr)
    }
}

fn as_callable(value: &Value) -> Option<Box<dyn LoxCallable>> {
    match value {
        Value::NativeFunction(f) => Some(Box::new(f.clone())),
        Value::LoxFunction(f) => Some(Box::new(f.clone())),
        _ => None,
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn ast_test1() -> result::Result<Value, ErrorType>  {
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
    fn ast_test2() -> result::Result<Value, ErrorType>  {
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
