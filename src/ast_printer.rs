
use crate::token::{Literal, Token, TokenType};
use crate::expr::{Expression, ExpressionVisitor};

// Test AST Printer Vistor implementation - not part of interpreter
#[derive(Default)]
pub struct AstPrinter { }

impl AstPrinter {
    pub fn print(&mut self, expression: Expression) -> String {
        self.evaluate(expression)
    }
}

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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn ast_printer() {
        // (* (- 123) (group 45.67))
        let binary: Expression = Expression::Binary(Box::new(Expression::Unary(Token {ttype: TokenType::Minus, lexeme: "-".into(), literal:None, line:1, col:5},
                                                                               Box::new(Expression::LiteralExpression(Literal::Number(123.))))),
                                                    Token {ttype: TokenType::Star, lexeme: "*".into(), literal:None, line:1, col:2},
                                                    Box::new(Expression::Grouping(Box::new(Expression::LiteralExpression(Literal::Number(45.67))))));
        let result = AstPrinter::default().print(binary);
        // println!("Result: {}", result);
        assert_eq!(result, "((- 123) * (group 45.67))")
    }
}
