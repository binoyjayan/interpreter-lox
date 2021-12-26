
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
    fn eval_assign(&mut self, name: Token, value: Box<Expression>) -> String {
        format!("{} = {}", name.lexeme, self.evaluate(*value)).into()
    }

    fn eval_binary(&mut self, left: Box<Expression>, operator: Token, right: Box<Expression>) -> String {
        format!("({} {} {})", self.evaluate(*left), operator.lexeme, self.evaluate(*right))
    }

    fn eval_grouping(&mut self, expr: Box<Expression>) -> String {
        format!("(group {})", self.evaluate(*expr))
    }

    fn eval_literal(&mut self, literal: Literal) -> String {
        format!("{}", literal)
    }

    fn eval_logical(&mut self, left: Box<Expression>, operator: Token, right: Box<Expression>) -> String {
        format!("({} {} {})", self.evaluate(*left), operator.lexeme, self.evaluate(*right))
    }

    fn eval_unary(&mut self, operator: Token, value: Box<Expression>) -> String {
        format!("({} {})", operator.lexeme, self.evaluate(*value))
    }

    fn eval_variable(&self, _expr: Token) -> String {
        "".into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn ast_test1() {
        //-123 * (45.67)   -->>    (* (- 123) (group 45.67))
        let binary: Expression = Expression::Binary(Box::new(Expression::Unary(Token {ttype: TokenType::Minus, lexeme: "-".into(), literal:None, line: 1, col: 1},
                                                                               Box::new(Expression::LiteralExpression(Literal::Number(123.))))),
                                                    Token {ttype: TokenType::Star, lexeme: "*".into(), literal: None, line: 1, col: 6},
                                                    Box::new(Expression::Grouping(Box::new(Expression::LiteralExpression(Literal::Number(45.67))))));
        let result = AstPrinter::default().print(binary);
        // println!("Result: {}", result);
        assert_eq!(result, "((- 123) * (group 45.67))")
    }

    #[test]
    fn ast_test2() {
        //-11.22 == (11.22)   -->>     (== (- 11.22) (group 11.22))
        let binary: Expression = Expression::Binary(Box::new(Expression::Unary(Token {ttype: TokenType::Minus, lexeme: "-".into(), literal:None, line: 2, col: 1},
                                                                               Box::new(Expression::LiteralExpression(Literal::Number(11.22))))),
                                                    Token {ttype: TokenType::EqualEqual, lexeme: "==".into(), literal: None, line: 2, col: 6},
                                                    Box::new(Expression::Grouping(Box::new(Expression::LiteralExpression(Literal::Number(11.22))))));
        let result = AstPrinter::default().print(binary);
        // println!("Result: {}", result);
        assert_eq!(result, "((- 11.22) == (group 11.22))")
    }
}
