use std::vec::Vec;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Variable,
    NumberLiteral,
    StringLiteral,
    Add,
    Subtract,
    Multiply,
    Divide,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Equals,
    Fn,
    Comma,
    Return
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String
}

pub struct Tokenizer<'a> {
    pub source: &'a str,
    pub tokens: Vec<Token>,
    pub cursor: usize
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            source,
            tokens: Vec::new(),
            cursor: 0
        }
    }

    fn ok(&self) -> bool {
        self.inbounds(0)
    }

    fn inbounds(&self, offset: usize) -> bool {
        (self.cursor + offset) < self.source.len()
    }

    fn peek(&self) -> char {
        if let Some(c) = self.source[self.cursor..].chars().next() {
            return c
        }

        return 0 as char
    }

    fn push_token(&mut self, kind: TokenKind, value: String) {
        self.tokens.push(Token{kind, value})
    }

    fn is_symbol(&mut self) -> bool {
        match self.peek() {
            '(' | ')' | '{' | '}' | '+' | '-' | '*' | '/' | '=' | ',' => true,
            _ => false
        }
    }

    pub fn tokenize(&mut self) {
        while self.ok() {
            let c = self.peek();

            if c.is_whitespace() {
                self.cursor += c.len_utf8();
                continue
            }

            if c.is_ascii_alphabetic() || c == '_' {
                self.consume_identifier()
            } else if c.is_numeric() {
                self.consume_number()
            } else if c == '"' {
                self.consume_string();
            } else if self.is_symbol() {
                self.consume_symbol();
            } else {
                println!("unknown symbol: {}", c);
                self.cursor += 1
            }
        }
    }

    fn consume_identifier(&mut self) {
        let mut ident = String::from("");

        while self.ok() && self.peek().is_ascii_alphabetic() || self.peek() == '_' {
            ident.push(self.peek());
            self.cursor += 1;
        }

        if let Some(keyword) = self.get_keyword(ident.clone()) {
            return self.push_token(keyword, ident);
        }

        self.push_token(TokenKind::Variable, ident);
    }

    fn consume_number(&mut self) {
        let mut number = String::new();

        while self.ok() && self.peek().is_numeric() || self.peek() == '.' {
            number.push(self.peek());
            self.cursor += 1;
        }

        self.push_token(TokenKind::NumberLiteral, number);
    }

    fn consume_string(&mut self) {
        self.cursor += 1;

        let mut str = String::new();

        while self.ok() && self.peek() != '"' {
            str.push(self.peek());
            self.cursor += 1;
        }

        self.cursor += 1;

        self.push_token(TokenKind::StringLiteral, str);
    }

    fn get_keyword(&mut self, keyword: String) -> Option<TokenKind> {
        match keyword.as_str() {
            "fn" => Some(TokenKind::Fn),
            "return" => Some(TokenKind::Return),
            _ => None
        }
    }

    fn consume_symbol(&mut self) {
        match self.peek() {
            '(' => self.push_token(TokenKind::OpenParen, '('.to_string()),
            ')' => self.push_token(TokenKind::CloseParen, ')'.to_string()),
            '{' => self.push_token(TokenKind::OpenCurly, '('.to_string()),
            '}' => self.push_token(TokenKind::CloseCurly, ')'.to_string()),
            '+' => self.push_token(TokenKind::Add, '+'.to_string()),
            '-' => self.push_token(TokenKind::Subtract, '-'.to_string()),
            '*' => self.push_token(TokenKind::Multiply, '*'.to_string()),
            '/' => self.push_token(TokenKind::Divide, '/'.to_string()),
            '=' => self.push_token(TokenKind::Equals, '='.to_string()),
            ',' => self.push_token(TokenKind::Comma, ','.to_string()),
            tok => println!("unknown token: {}", tok)
        }

        self.cursor += 1;
    }
}