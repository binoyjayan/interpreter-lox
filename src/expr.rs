use anyhow::Result;

use crate::token::{Literal, Token};

#[allow(dead_code)]
#[derive(PartialEq, Clone, Debug)]
pub enum Expression {
    Assign(Token, Box<Expression>),
    Binary(Box<Expression>, Token, Box<Expression>),
    Grouping(Box<Expression>),
    LiteralExpression(Literal),
    LogicalExpression(Box<Expression>, Token, Box<Expression>),
    Unary(Token, Box<Expression>),
    Variable(Token),
}

pub trait ExpressionVisitor<T> {
    fn evaluate(&mut self, expression: Expression) -> Result<T> {
        match expression {
            Expression::Assign(name, value) => self.eval_assign(name, value),
            Expression::Binary(b1, o, b2) => self.eval_binary(b1, o, b2),
            Expression::Grouping(g) => self.eval_grouping(g),
            Expression::LiteralExpression(l) => self.eval_literal(l),
            Expression::Unary(operator, right) => self.eval_unary(operator, right),
            Expression::Variable(v) => self.eval_variable(v),
            Expression::LogicalExpression(left, operator, right) => self.eval_logical(left, operator, right),
        }
    }
    fn eval_assign(&mut self, name: Token, value: Box<Expression>) -> Result<T>;
    fn eval_binary(&mut self, left: Box<Expression>, operator: Token, right: Box<Expression>) -> Result<T>;
    fn eval_grouping(&mut self, expression: Box<Expression>) -> Result<T>;
    fn eval_literal(&mut self, literal: Literal) -> Result<T>;
    fn eval_logical(&mut self, left: Box<Expression>, operator: Token, right: Box<Expression>) -> Result<T>;
    fn eval_unary(&mut self, operator: Token, value: Box<Expression>) -> Result<T>;
    fn eval_variable(&self, expression: Token) -> Result<T>;
}
