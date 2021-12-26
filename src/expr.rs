use crate::token::{Literal, Token, TokenType};

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
    fn evaluate(&mut self, expression: Expression) -> T {
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
    fn eval_assign(&mut self, name: Token, value: Box<Expression>) -> T;
    fn eval_binary(&mut self, left: Box<Expression>, operator: Token, right: Box<Expression>) -> T;
    fn eval_grouping(&mut self, expression: Box<Expression>) -> T;
    fn eval_literal(&mut self, literal: Literal) -> T;
    fn eval_logical(&mut self, left: Box<Expression>, operator: Token, right: Box<Expression>) -> T;
    fn eval_unary(&mut self, operator: Token, value: Box<Expression>) -> T;
    fn eval_variable(&self, expression: Token) -> T;
}

#[allow(dead_code)]
#[derive(PartialEq, Clone, Debug)]
pub enum Statement {
    Block(Vec<Statement>),
    Expression(Expression),
    If(Expression, Box<Statement>, Box<Option<Statement>>),
    Print(Expression),
    Var(Token, Option<Expression>),
    While(Expression, Box<Statement>)
}

pub trait StatementVisitor<T> {
    fn execute(&mut self, statement: Statement) -> T {
        match statement {
            Statement::If(condition, then_branch, else_branch) => {
                self.exec_if_stmt(condition, then_branch, else_branch)
            }
            Statement::Block(statements) => self.exec_block(statements),
            Statement::Expression(statement) => self.exec_expr(statement),
            Statement::Print(statement) => self.exec_print(statement),
            Statement::Var(name, initializer) => self.exec_var(name, initializer),
            Statement::While(condition, body) => self.exec_while(condition, body),
        }
    }
    fn exec_if_stmt(&mut self, condition: Expression,
                            then_branch: Box<Statement>,
                            else_branch: Box<Option<Statement>>) -> T;
    fn exec_block(&mut self, statements: Vec<Statement>) -> T;
    fn exec_expr(&mut self, statement: Expression) -> T;
    fn exec_print(&mut self, statement: Expression) -> T;
    fn exec_var(&mut self, name: Token, initializer: Option<Expression>) -> T;
    fn exec_while(&mut self, condition: Expression, body: Box<Statement>) -> T;
}
