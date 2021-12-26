
// The AST Tree-walk Interpreter
use crate::value::Value;
use crate::token::{Literal, Token, TokenType};
use crate::expr::{Expression, ExpressionVisitor, Statement};

#[derive(Default)]
pub struct Interpreter { }

impl Interpreter {
    pub fn interpret(&mut self, expression: Expression) -> Value {
        self.evaluate(expression)
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
}

impl ExpressionVisitor<Value> for Interpreter {
    fn eval_assign(&mut self, name: Token, value: Box<Expression>) -> Value {
        Value::Number(111.222)
    }

    // Evaluate left and right subexpressions first and then perform arithmetic,
    // logical or equality operations. The arithmetic operation produces result
    // whose type is same as  the operands. However, the logical and equality
    // operators produce a boolean result.
    fn eval_binary(&mut self, left: Box<Expression>, operator: Token, right: Box<Expression>) -> Value {
        let left = self.evaluate(*left);
        let right = self.evaluate(*right);
        // println!("-------------------- Operator: {:?} Left: {}, Right: {}", operator.ttype, left, right);
        match operator.ttype {
            TokenType::Minus => {
                if let (Value::Number(l), Value::Number(r)) = (&left, &right) {
                    Value::Number(l - r)
                } else {
                    println!("Subtraction not allowed on '{}' and '{}'", left, right);
                    Value::Nil
                }
            },
            TokenType::Slash => {
                if let (Value::Number(l), Value::Number(r)) = (left.clone(), right.clone()) {
                    Value::Number(l / r)
                } else {
                    println!("Division not allowed on '{}' and '{}'", left, right);
                    Value::Nil
                }
            },
            TokenType::Star => {
                if let (Value::Number(l), Value::Number(r)) = (left.clone(), right.clone()) {
                    Value::Number(l * r)
                } else {
                    println!("Multiplication not allowed on '{}' and '{}'", left, right);
                    Value::Nil
                }
            },
            TokenType::Plus => {
                // Plus is special as it can handle both numberic as well as strings
                if let (Value::Number(l), Value::Number(r)) = (left.clone(), right.clone()) {
                    Value::Number(l + r)
                } else if let (Value::Str(l), Value::Str(r)) = (left.clone(), right.clone()) {
                    Value::Str(l + &r)
                } else {
                    println!("Addition not allowed on '{}' and '{}'", left, right);
                    Value::Nil
                }
            },
            TokenType::Greater => {
                if let (Value::Number(l), Value::Number(r)) = (left.clone(), right.clone()) {
                    Value::Bool(l > r)
                } else {
                    println!("Relational operation not allowed on '{}' and '{}'", left, right);
                    Value::Nil
                }
            },
            TokenType::GreaterEqual => {
                if let (Value::Number(l), Value::Number(r)) = (left.clone(), right.clone()) {
                    Value::Bool(l >= r)
                } else {
                    println!("Relational operation not allowed on '{}' and '{}'", left, right);
                    Value::Nil
                }
            },
            TokenType::Less => {
                if let (Value::Number(l), Value::Number(r)) = (left.clone(), right.clone()) {
                    Value::Bool(l < r)
                } else {
                    println!("Relational operation not allowed on '{}' and '{}'", left, right);
                    Value::Nil
                }
            },
            TokenType::LessEqual => {
                if let (Value::Number(l), Value::Number(r)) = (left.clone(), right.clone()) {
                    Value::Bool(l <= r)
                } else {
                    println!("Relational operation not allowed on '{}' and '{}'", left, right);
                    Value::Nil
                }
            },
            TokenType::BangEqual => Value::Bool(!Self::is_equal(left, right)),
            TokenType::EqualEqual => Value::Bool(Self::is_equal(left, right)),
            _ => Value::Nil,
        }
    }

    // Send the grouped expression back into the interpreter’s visitor implementation
    fn eval_grouping(&mut self, expr: Box<Expression>) -> Value {
        self.evaluate(*expr)
    }

    // Simplest all expression. Just convert the literal to a 'Value'
    // Do not call this when an identifier is encountered.
    fn eval_literal(&mut self, literal: Literal) -> Value {
        literal.into()
    }

    fn eval_logical(&mut self, left: Box<Expression>, operator: Token, right: Box<Expression>) -> Value {
        Value::Bool(false)
    }

    // unary expressions have a single subexpression must be evaluated first
    // Then apply the unary operator itself to the result. Here, the minus ('-')
    // operator negates the subexpression, whereas the Bang ('!') operator 
    // inverts the truth value.
    fn eval_unary(&mut self, operator: Token, value: Box<Expression>) -> Value {
        let value = self.evaluate(*value);
        match operator.ttype {
            TokenType::Minus => {
                if let Value::Number(n) = value {
                    Value::Number(-n)
                } else {
                    println!("Negation operation is not allowed on '{}'", value);
                    Value::Nil
                }
            },
            TokenType::Bang => Value::Bool(!Self::is_truthy(&value)),
            _ => Value::Nil,
        }
    }

    fn eval_variable(&self, _expr: Token) -> Value {
        Value::Nil
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn ast_test1() {
        //-12.34 + (56.78)   --->> 44.44
        let binary: Expression = Expression::Binary(Box::new(Expression::Unary(Token {ttype: TokenType::Minus, lexeme: "-".into(), literal:None, line: 1, col: 1},
                                                                               Box::new(Expression::LiteralExpression(Literal::Number(12.34))))),
                                                    Token {ttype: TokenType::Plus, lexeme: "+".into(), literal: None, line: 1, col: 6},
                                                    Box::new(Expression::Grouping(Box::new(Expression::LiteralExpression(Literal::Number(56.78))))));
        let result: Value = Interpreter::default().interpret(binary);
        // println!("Result: {}", result);
        assert_eq!(format!("{}", result), format!("{}", Value::Number(44.44)));
    }

    #[test]
    fn ast_test2() {
        //-11.22 == (-11.22)  --->> true
        let binary: Expression = Expression::Binary(Box::new(Expression::Unary(Token {ttype: TokenType::Minus, lexeme: "-".into(), literal:None, line: 2, col: 1},
                                                                               Box::new(Expression::LiteralExpression(Literal::Number(11.22))))),
                                                    Token {ttype: TokenType::EqualEqual, lexeme: "==".into(), literal: None, line: 2, col: 8},
                                                    Box::new(Expression::Grouping(Box::new(Expression::LiteralExpression(Literal::Number(-11.22))))));
        let result = Interpreter::default().interpret(binary);
        // println!("Result: {}", result);
        assert_eq!(format!("{}", result), format!("{}", Value::Bool(true)));
    }
}
