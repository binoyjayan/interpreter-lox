use anyhow::Result;

use crate::token::Token;
use crate::expr::Expression;

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
    fn execute(&mut self, statement: Statement) -> Result<T> {
        match statement {
            Statement::If(condition, then_branch, else_branch) => {
                self.exec_if_stmt(condition, then_branch, else_branch)
            },
            Statement::Block(statements) => self.exec_block(statements),
            Statement::Expression(statement) => self.exec_expr(statement),
            Statement::Print(statement) => self.exec_print(statement),
            Statement::Var(name, initializer) => self.exec_var(name, initializer),
            Statement::While(condition, body) => self.exec_while(condition, body),
        }
    }
    fn exec_if_stmt(&mut self, condition: Expression,
                            then_branch: Box<Statement>,
                            else_branch: Box<Option<Statement>>) -> Result<T>;
    fn exec_block(&mut self, statements: Vec<Statement>) -> Result<T>;
    fn exec_expr(&mut self, statement: Expression) -> Result<T>;
    fn exec_print(&mut self, statement: Expression) -> Result<T>;
    fn exec_var(&mut self, name: Token, initializer: Option<Expression>) -> Result<T>;
    fn exec_while(&mut self, condition: Expression, body: Box<Statement>) -> Result<T>;
}
