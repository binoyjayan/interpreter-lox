use std::result;
use crate::token::Token;
use crate::expr::Expression;

#[allow(dead_code)]
#[derive(PartialEq, Clone, Debug)]
pub enum Statement {
    Block(Vec<Statement>),
    Expression(Expression),
    Print(Expression),
    Return(Option<Expression>),
    Var(Token, Option<Expression>),
    Fun(Token, Vec<Token>, Box<Statement>),
    While(Expression, Box<Statement>),
    If(Expression, Box<Statement>, Box<Option<Statement>>),
}


pub trait StatementVisitor<T, E> {
    fn execute(&mut self, statement: Statement) -> result::Result<T, E> {
        match statement {
            Statement::Block(statements) => self.exec_block(statements),
            Statement::Expression(statement) => self.exec_expr(statement),
            Statement::Print(statement) => self.exec_print(statement),
            Statement::Return(retval) => self.exec_return(retval),
            Statement::Var(name, initializer) => self.exec_var(name, initializer),
            Statement::Fun(name, params, body) => self.exec_fun(name, params, body),
            Statement::If(condition, then_branch, else_branch) => {
                self.exec_if_stmt(condition, then_branch, else_branch)
            },
            Statement::While(condition, body) => self.exec_while(condition, body),
        }
    }
    fn exec_if_stmt(&mut self, condition: Expression,
                            then_branch: Box<Statement>,
                            else_branch: Box<Option<Statement>>) -> result::Result<T, E>;
    fn exec_block(&mut self, statements: Vec<Statement>) -> result::Result<T, E>;
    fn exec_expr(&mut self, statement: Expression) -> result::Result<T, E>;
    fn exec_print(&mut self, statement: Expression) -> result::Result<T, E>;
    fn exec_return(&mut self, retval: Option<Expression>) -> result::Result<T, E>;
    fn exec_var(&mut self, name: Token, initializer: Option<Expression>) -> result::Result<T, E>;
    fn exec_fun(&mut self, name: Token, params: Vec<Token>, body: Box<Statement>) -> result::Result<T, E>;
    fn exec_while(&mut self, condition: Expression, body: Box<Statement>) -> result::Result<T, E>;
}
