pub use crate::token::*;

pub struct Scanner {
    source: Vec<char>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    col: usize,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            source: source.chars().collect(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            col: 0,
        }
    }

    pub fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    pub fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '('  => self.add_token(TokenType::LeftParen, None),
            ')'  => self.add_token(TokenType::RightParen, None),
            '{'  => self.add_token(TokenType::LeftBrace, None),
            '}'  => self.add_token(TokenType::RightBrace, None),
            ','  => self.add_token(TokenType::Comma, None),
            '.'  => self.add_token(TokenType::Dot, None),
            '-'  => self.add_token(TokenType::Minus, None),
            '+'  => self.add_token(TokenType::Plus, None),
            ';'  => self.add_token(TokenType::Semicolon, None),
            '/'  => self.add_token(TokenType::Slash, None),
            '*'  => self.add_token(TokenType::Star, None),
            '!'  => self.add_token_twin('=', TokenType::BangEqual, TokenType::Bang),
            '='  => self.add_token_twin('=', TokenType::EqualEqual, TokenType::Equal),
            '<'  => self.add_token_twin('=', TokenType::LessEqual, TokenType::Less),
            '>'  => self.add_token_twin('=', TokenType::GreaterEqual, TokenType::Greater),
            '/'  => self.handle_slash(),
            ' ' | '\r' | '\t' => {}
            '\n' => {
                self.line += 1;
                self.col = 0;
            }
            '"' => self.handle_string(),
            _ => println!("Unknown token {}", c)
        }
    }

    pub fn scan_tokens(&mut self) -> &Vec<Token> {
        while ! self.is_at_end() {
            // We are at the beginning of the next lexeme
            self.start = self.current;
            self.scan_token();
        }
    
        self.add_token(TokenType::Eof, None);
        &self.tokens
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.col += 1;
        self.source[self.current - 1]
    }

    // Generic function for all tokens. Should have wrapper around it for identifier, numeric, etc
    fn add_token(&mut self, ttype: TokenType, literal: Option<Literal>) {
        let lexeme: String = self.source[self.start..self.current].iter().collect();
        self.tokens.push(
            Token{
                ttype,
                lexeme,
                literal,
                line: self.line,
                col: self.col,
            }
        );
    }
    // Process two character tokens [ == != ]
    fn add_token_twin(&mut self, next: char, twin_type: TokenType, single_type: TokenType) {
        let matches_second = self.matches(next);
        self.add_token(if matches_second { twin_type } else { single_type }, None);
    }

    // Handle slash character separately since it can be comment or a division operator
    fn handle_slash(&mut self) {
        if self.matches('/') {
            while self.peek() != '\n' && !self.is_at_end() {
                self.advance();
            }
        } else {
            self.add_token(TokenType::Slash, None)
        }
    }

    fn handle_string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1
            }
            self.advance();
        }

        if self.is_at_end() {
            println!("Unterminated string at line {}", self.line)
        }

        self.advance();

        let s = self.source[self.start + 1..self.current - 1].iter().collect();
        self.add_token(TokenType::StringLiteral, Some(Literal::Str(s)))
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source[self.current] != expected {
            return false;
        }
        // if expected, advance
        self.current += 1;
        self.col += 1;
        true
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source[self.current + 1]
        }
    }

    // lookahead - similar to advance but does not consume the character
    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.current]
        }
    }

}
