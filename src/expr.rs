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
            Expression::Assign(name, value) => self.execute_assign_expression(name, value),
            Expression::Binary(b, o, b2) => self.execute_binary_expression(b, o, b2),
            Expression::Grouping(g) => self.execute_grouping_expression(g),
            Expression::LiteralExpression(l) => self.execute_literal_expression(l),
            Expression::Unary(operator, right) => self.execute_unary_expression(operator, right),
            Expression::Variable(v) => self.execute_variable_expression(v),
            Expression::LogicalExpression(left, operator, right) => self.execute_logical_expression(left, operator, right),
        }
    }
    fn execute_assign_expression(&mut self, name: Token, value: Box<Expression>) -> T;
    fn execute_binary_expression(&mut self, left: Box<Expression>, operator: Token, right: Box<Expression>) -> T;
    fn execute_grouping_expression(&mut self, expression: Box<Expression>) -> T;
    fn execute_literal_expression(&mut self, literal: Literal) -> T;
    fn execute_logical_expression(&mut self, left: Box<Expression>, operator: Token, right: Box<Expression>) -> T;
    fn execute_unary_expression(&mut self, operator: Token, value: Box<Expression>) -> T;
    fn execute_variable_expression(&self, expression: Token) -> T;
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
                self.execute_if_statement(condition, then_branch, else_branch)
            }
            Statement::Block(statements) => self.execute_block_statement(statements),
            Statement::Expression(statement) => self.execute_expression_statement(statement),
            Statement::Print(statement) => self.execute_print_statement(statement),
            Statement::Var(name, initializer) => self.execute_var_statement(name, initializer),
            Statement::While(condition, body) => self.execute_while_statement(condition, body),
        }
    }
    fn execute_if_statement(&mut self, condition: Expression,
                            then_branch: Box<Statement>,
                            else_branch: Box<Option<Statement>>) -> T;
    fn execute_block_statement(&mut self, statements: Vec<Statement>) -> T;
    fn execute_expression_statement(&mut self, statement: Expression) -> T;
    fn execute_print_statement(&mut self, statement: Expression) -> T;
    fn execute_var_statement(&mut self, name: Token, initializer: Option<Expression>) -> T;
    fn execute_while_statement(&mut self, condition: Expression, body: Box<Statement>) -> T;
}

#[cfg(test)]
mod tests {
    use super::*;
    #[derive(Default)]
    // Test AST Printer Vistor implementation - not part of interpreter
    struct AstPrinter {  }

    impl ExpressionVisitor<String> for AstPrinter {
        fn execute_assign_expression(&mut self, name: Token, value: Box<Expression>) -> String {
            format!("{} = {}", name.lexeme, self.evaluate(*value)).into()
        }

        fn execute_binary_expression(&mut self, left: Box<Expression>, operator: Token, right: Box<Expression>) -> String {
            format!("({} {} {})", self.evaluate(*left), operator.lexeme, self.evaluate(*right))
        }

        fn execute_grouping_expression(&mut self, expr: Box<Expression>) -> String {
            format!("(group {})", self.evaluate(*expr))
        }

        fn execute_literal_expression(&mut self, literal: Literal) -> String {
            format!("{}", literal)
        }

        fn execute_logical_expression(&mut self, _left: Box<Expression>, _operator: Token, _right: Box<Expression>) -> String {
            "".into()
        }

        fn execute_unary_expression(&mut self, operator: Token, value: Box<Expression>) -> String {
            format!("({} {})", operator.lexeme, self.evaluate(*value))
        }

        fn execute_variable_expression(&self, _expr: Token) -> String {
            "".into()
        }
    }

    #[test]
    fn ast_printer() {
        // (* (- 123) (group 45.67))
        let binary: Expression = Expression::Binary(Box::new(Expression::Unary(Token {ttype:TokenType::Minus, lexeme: "-".into(), literal:None, line:1, col:5},
                                                                               Box::new(Expression::LiteralExpression(Literal::Number(123.))))),
                                                    Token {ttype: TokenType::Star, lexeme: "*".into(), literal:None, line:1, col:2},
                                                    Box::new(Expression::Grouping(Box::new(Expression::LiteralExpression(Literal::Number(45.67))))));
        
        let mut astprinter: AstPrinter = AstPrinter::default();
        let result = astprinter.evaluate(binary);

        // println!("Result: {}", result);
        assert_eq!(result, "((- 123) * (group 45.67))")
    }
}
