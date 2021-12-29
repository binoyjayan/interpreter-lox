use anyhow::{anyhow, Result};

use crate::stmt::{Statement};
use crate::expr::{Expression};
use crate::function::MAX_ARITY;
use crate::token::{Literal, Token, TokenType};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

// Grammar for expressions:

// When the first symbol in the body of the rule is the same as
// the head of the rule means that the production is left-recursive
// Use left recursive production rules for left-associative operations,
// and right recursive rules for right-associative operations.

// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                  | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                  | "(" expression ")" ;
// 
// Terminal	       Code to match and consume a token
// Nonterminal	   Call to that rule’s function
// |               if or switch statement
// * or +          while or for loop
// ?               if statement


impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0}
    }

    pub fn parse(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            if let Ok(statement) = self.declaration() {
                statements.push(statement);
            }
        }
        statements
    }

    // Variable declarations are statements, but they are different
    // from other statements in the sense that they cannot be single
    // statements. i.e. the following is not valid:
    // if (condition) var myvar = "myvalue";
    fn declaration(&mut self) -> Result<Statement> {
        if self.matches(&[TokenType::Var]) {
            if let Ok(statement) = self.var_declaration() {
                Ok(statement)
            } else {
                self.synchronize();
                Err(anyhow!("Error in variable declaration"))
            }
        } else if self.matches(&[TokenType::Fun]) {
            if let Ok(statement) = self.fun_declaration("function".into()) {
                Ok(statement)
            } else {
                self.synchronize();
                Err(anyhow!("Error in function declaration"))
            }
        } else if let Ok(statement) = self.statement() {
            Ok(statement)
        } else {
            self.synchronize();
            Err(anyhow!("Error in declaration"))
        }
    }

    // A variable statement doesn’t just define a new variable,
    // it can also be used to redefine an existing variable.
    // This helps the development in a REPL session in not having
    // to mentally track which variables are already defined.
    fn var_declaration(&mut self) -> Result<Statement> {
        let name = self.consume(&TokenType::Identifier, "Expect variable name.")?;
        let initializer = if self.matches(&[TokenType::Equal]) {
            self.expression().ok()
        } else {
            // No initializer - default value for variable is None
            None
        };
        self.consume(&TokenType::Semicolon, "Expect ';' after variable declaration.")?;
        Ok(Statement::Var(name, initializer))
    }

    // A function declaration statement is the fun keyword followed by a
    // function name, parameters, and function body. This method is also
    // used for parsing class methods. The 'kind' is used to differentiate
    // between functions and methods.
    fn fun_declaration(&mut self, kind: String) -> Result<Statement> {
        let name = self.consume(&TokenType::Identifier, &format!("Expect {} name.", kind))?;
        self.consume(&TokenType::LeftParen, &format!("Expect '(' after {} name.", kind))?;
        let mut parameters = Vec::new();
        // If the first token after '(' is ')' then there are no parameters
        if !self.check(&TokenType::RightParen) {
            while !self.is_at_end() {
                if parameters.len() >= MAX_ARITY {
                    crate::error_at_token(&self.peek(), &format!("Can't have more than {} parameters", MAX_ARITY));
                }

                let param = self.consume(&TokenType::Identifier, "Expect a parameter name.")?;
                parameters.push(param);
                if !self.matches(&[TokenType::Comma]) {
                    // end of parameters list
                    break;
                }
            }
            self.consume(&TokenType::RightParen, &format!("Expect ')' after {} parameters.", kind))?;
        }
        // Parse function body which a block statement
        // Note that the token '{' is consumed at the beginning of the body
        // before calling block_statement(). That’s because block_statement()
        // assumes that the brace token has already been matched.
        self.consume(&TokenType::LeftBrace, &format!("Expect '{{' for {} body.", kind))?;
        let body = self.block_statement()?;
        Ok(Statement::Fun(name, parameters, Box::new(body)))
    }

    fn statement(&mut self) -> Result<Statement> {
        if self.matches(&[TokenType::For]) {
            self.for_statement()
        } else if self.matches(&[TokenType::If]) {
            self.if_statement()
        } else if self.matches(&[TokenType::Print]) {
            self.print_statement()
        } else if self.matches(&[TokenType::Return]) {
            self.return_statement()
        } else if self.matches(&[TokenType::While]) {
            self.while_statement()
        } else if self.matches(&[TokenType::LeftBrace]) {
            self.block_statement()
        } else {
            self.expression_statement()
        }
    }

    // For loops are implemented by reducing it to a while loop by a process
    // called syntactic desugaring. For example, the following for loop may be
    // reduced to a while loop and the interpreter does not have to know about it.
    // for (initializer; condition; step) { block_stmt }
    // { initializer; while(condition) { block_stmt; step; } }
    // Note that the while loop itself is now part of a block statement

    fn for_statement(&mut self) -> Result<Statement> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'for'.")?;

        // parse for loop initializer (before first semicolon)
        let initializer = if self.matches(&[TokenType::Semicolon]) {
            None
        } else if self.matches(&[TokenType::Var]) {
            // if it also has a variable declaration
            self.var_declaration().ok()
        } else {
            self.expression_statement().ok()
        };

        // parse for loop condition
        let condition = if self.check(&TokenType::Semicolon) {
            // If not condition, the it is assumed to be 'true'
            Expression::LiteralExpression(Literal::Bool(true))
        } else {
            self.expression()?
        };
        self.consume(&TokenType::Semicolon, "Expect ';' after a for loop condition.")?;

        // parse for loop step
        let step = if self.check(&TokenType::RightParen) {
            None
        } else {
            self.expression().ok()
        };
        self.consume(&TokenType::RightParen, "Expect ')' after a for loop step.")?;

        // parse for loop body
        let mut body = self.statement()?;
        // If a step was mentioned, add it to the for loops body as the last statement.
        if let Some(step) = step {
            // this is the body of the while loop
            body = Statement::Block(vec![body, Statement::Expression(step)]);
        }
        // Now, wrap it in a while statement for the interpreter to consume
        let mut while_stmt = Statement::While(condition, Box::new(body));

        // if there is an initializer, then the 'while' statement itself will
        // be wrapped with a block statement that has the initializer as the first
        // statement and the while loop as the next.
        if let Some(initializer) = initializer {
            while_stmt = Statement::Block(vec![initializer, while_stmt]);
        }
        Ok(while_stmt)
    }

    // the parser recognizes an if statement by the leading if keyword.
    fn if_statement(&mut self) -> Result<Statement> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'if'.")?;
        // expression that is evaluated
        let condition = self.expression()?;
        self.consume(&TokenType::RightParen, "Expect ')' after if condition.")?;
        let then_branch = self.statement()?;
        // Solving dangling else problem in case of inner and outer if statements
        // but with only one else statement. If that happens, eagerly look for an
        // else before returning, the innermost call to a nested series will claim
        // the else clause for itself before returning to the outer if statements.
        let else_branch = if self.matches(&[TokenType::Else]) {
            self.statement().ok()
        } else {
            // If there is no else branch, the else_branch field in the syntax tree is None
            None
        };
        Ok(Statement::If(condition, Box::new(then_branch), Box::new(else_branch)))
    }
    
    fn print_statement(&mut self) -> Result<Statement> {
        let value = self.expression()?;
        self.consume(&TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Statement::Print(value))
    }

    fn return_statement(&mut self) -> Result<Statement> {
        if !self.check(&TokenType::Semicolon) {
            let value = self.expression()?;
            self.consume(&TokenType::Semicolon, "Expect ';' after value.")?;
            Ok(Statement::Return(Some(value)))
        } else {
            Ok(Statement::Return(None))
        }
    }

    fn while_statement(&mut self) -> Result<Statement> {
        self.consume(&TokenType::LeftParen, "Expect '(' after a while statement.")?;
        let condition = self.expression()?;
        self.consume(&TokenType::RightParen, "Expect ')' after a while condition.")?;
        let body = self.statement()?;
        Ok(Statement::While(condition, Box::new(body)))
    }

    // A block is a (possibly empty) series of statements or declarations
    // surrounded by curly braces and can appear anywhere a statement is allowed.
    // Note that the token '{' is already consumed when this function is called.
    fn block_statement(&mut self) -> Result<Statement> {
        let mut statements = Vec::new();

        while !self.is_at_end() && !self.check(&TokenType::RightBrace) {
            if let Ok(statement) = self.declaration() {
                statements.push(statement);
            }
        }
        self.consume(&TokenType::RightBrace, "Expect '}' after block.")?;
        Ok(Statement::Block(statements))
    }

    // This is an expression followed by a semicolon
    fn expression_statement(&mut self) -> Result<Statement> {
        let expr = self.expression()?;
        self.consume(&TokenType::Semicolon, "Expect ';' after expression.")?;
        Ok(Statement::Expression(expr))
    }

    // The first rule, expression, simply expands to the assignment rule
    fn expression(&mut self) -> Result<Expression> {
        self.assignment()
    }

    // The assignment expression is unique in the sense that it
    // has an l-value unlike only r-value for other expressions.
    // An l-value refers to a storage location that can be assigned to.
    // The syntax tree should reflect that an l-value isn’t evaluated
    // like a normal expression. Because of that, the node 'Expression::Assign'
    // has a 'Token' for the left-hand side, and not of type 'Expression'.
    // But the parser doesn’t know it’s parsing an l-value until it encounters
    // an '=' operator. In a complex l-value, that can occur many tokens later.
    // e.g. func().b.c = value;
    // The parsing looks similar to that of the other binary operators like +.
    // The the lhs is parsed, which can be any expression of higher precedence.
    // If a '=' is found, parse the rhs and then wrap it all up in an assignment
    // expression tree node.
    fn assignment(&mut self) -> Result<Expression> {
        let expr = self.logical_or()?;

        // match and consume the assignment operator '='
        if self.matches(&[TokenType::Equal]) {
            let equals = self.previous();
            // Since assignment is right-associative, recursively call assignment()
            // to parse the right-hand side.
            let value = self.assignment()?;
            // Verify that the type of the assignment target is a variable
            if let Expression::Variable(name) = expr {
                Ok(Expression::Assign(name, Box::new(value)))
            } else {
                crate::error_at_token(&equals, "Invalid assignment target");
                Err(anyhow!("Parse error"))
            }
        } else {
            Ok(expr)
        }
    }

    // The logcal operators aren’t like other binary operators because
    // they short-circuit. <condition> && expression();
    // That is the reason why 'and' and 'or' are treated separately.
    fn logical_or(&mut self) -> Result<Expression> {
        let mut expr = self.logical_and()?;
        // Parse a series of or expressions
        while self.matches(&[TokenType::Or]) {
            let operator = self.previous();
            // Its operands are the next higher level of precedence, the new 'and' expression.
            let right = self.logical_and()?;
            expr = Expression::LogicalExpression(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expression> {
        let mut expr = self.equality()?;
        // Parse a series of and expressions
        while self.matches(&[TokenType::And]) {
            let operator = self.previous();
            // call equality as the next higher level of precedence
            let right = self.equality()?;
            expr = Expression::LogicalExpression(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expression> {
        let mut expr = self.comparison()?;

        // The ( ... )* loop in the rule maps to a while loop.
        // Grab the matched operator token to track which kind
        // of equality expression is available. Then call comparison()
        // again to parse the right-hand operand. Combine the operator
        // and its two operands into a new 'Expression::Binary' syntax
        // tree node, and then loop around. For each iteration, store
        // the resulting expression back in the same expr local variable.
        // By zipping through a sequence of equality expressions, a
        // left-associative nested tree of binary operator is nodes created.
        // If an an equality operator is not found, break the loop.

        while self.matches(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression> {
        let mut expr = self.term()?;
        let compare_operators = [
            TokenType::Greater, TokenType::GreaterEqual,
            TokenType::Less, TokenType::LessEqual
        ];
        while self.matches(&compare_operators) {
            let operator = self.previous();
            let right = self.term()?;
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    // In order of precedence, first addition and subtraction
    fn term(&mut self) -> Result<Expression> {
        let mut expr = self.factor()?;
        while self.matches(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous();
            let right = self.factor()?;
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    // Multiplication and then division
    fn factor(&mut self) -> Result<Expression> {
        let mut expr = self.unary()?;
        while self.matches(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    // If encountered a unary operator, recursively call unary 
    // recursively again to parse the expression.
    fn unary(&mut self) -> Result<Expression> {
        if self.matches(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.unary()?;
            return Ok(Expression::Unary(operator, Box::new(right)))
        }
        self.call()
    }

    fn call(&mut self) -> Result<Expression> {
        // the callee itself is not the function name but is an expression
        // that evaluates to a function name
        let mut expr = self.primary()?;

        // Each time a '(' is seen call finish_call() to parse the
        // call expression using the previously parsed expression
        // as the callee. The returned expression becomes the new
        // expr and loop to see if the result is itself called.
        while self.matches(&[TokenType::LeftParen]) {
            expr = self.finish_call(expr)?;
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expression) -> Result<Expression> {
        let mut arguments = Vec::new();

        // If the first token after '(' is ')' then there are no arguments
        if !self.check(&TokenType::RightParen) {
            while !self.is_at_end() {
                if arguments.len() >= MAX_ARITY {
                    crate::error_at_token(&self.peek(), &format!("Can't have more than {} parameters", MAX_ARITY));
                }
                if let Ok(expression) = self.expression() {
                    arguments.push(expression);
                }
                if !self.matches(&[TokenType::Comma]) {
                    // end of arguments list
                    break;
                }
            }
        }
        self.consume(&TokenType::RightParen, "Expect `)` after arguments")?;
        Ok(Expression::Call(Box::new(callee), arguments))
    }

    // Reached highest level of precedence after crawling up the
    // precedence hierarchy. Most of the primary rules are terminals.
    fn primary(&mut self) -> Result<Expression> {
        if self.matches(&[TokenType::False]) {
            return Ok(Expression::LiteralExpression(Literal::Bool(false)));
        }
        if self.matches(&[TokenType::True]) {
            return Ok(Expression::LiteralExpression(Literal::Bool(true)));
        }
        if self.matches(&[TokenType::Nil]) {
            return Ok(Expression::LiteralExpression(Literal::Nil));
        }
        if self.matches(&[TokenType::Number, TokenType::StringLiteral]) {
            return Ok(Expression::LiteralExpression(match self.previous().literal {
                Some(l) => l,
                None => Literal::Nil,
            }));
        }
        if self.matches(&[TokenType::Identifier]) {
            return Ok(Expression::Variable(self.previous()));
        }
        if self.matches(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(&TokenType::RightParen, "Expect `)` after expression")?;
            return Ok(Expression::Grouping(Box::new(expr)));
        }
        // Encountered a token that can’t start an expression.
        crate::error_at_token(&self.peek(), "Expect expression");
        Err(anyhow!("Parse error"))
    }

    // Synchronize the recursive descent parser by discarding
    // token right until the beginning of the next statement
    // i.e. when a semicolon or any of the special keywords is seen.

    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if self.previous().ttype == TokenType::Semicolon {
                return;
            }
            match self.peek().ttype {
                TokenType::Class | 
                TokenType::Fun | 
                TokenType::Var |
                TokenType::For |
                TokenType::If |
                TokenType::While |
                TokenType::Print |
                TokenType::Return => {
                    return;
                }
                _ => {}
            }
            self.advance();
        }
    }

    // Check to see if the current token has any of the given types.
    // If so, consume the token and return true
    fn matches(&mut self, types: &[TokenType]) -> bool {
        for ttype in types {
            if self.check(ttype) {
                self.advance();
                return true;
            }
        }
        false
    }

    // Similar to match() in that it checks to see if the next token is
    // of the expected type. If so, it consumes the token.
    // If some other token is encountered, report an error.
    fn consume(&mut self, ttype: &TokenType, message: &str) -> Result<Token> {
        if self.check(ttype) {
            Ok(self.advance())
        } else {
            crate::error_at_token(&self.peek(), message);
            Err(anyhow!("Parse error"))
        }
    }

    // Returns true if the current token is of the given type
    fn check(&self, ttype: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        &self.peek().ttype == ttype
    }

    // Consumes the current token and return it
    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    // check if we have run out of tokens
    fn is_at_end(&self) -> bool {
        self.peek().ttype == TokenType::Eof
    }

    // return the current token yet to be consumed
    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    // return the most recently consumed token
    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }
}
