use std::vec::Vec;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Identifier,
    NumberLiteral,
    StringLiteral,
    TrueLiteral,
    FalseLiteral,
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
    Return,
    U8, U16, U32, U64,
    I8, I16, I32, I64, 
    F32, F64,
    Void,
    Byte,
    Bool,
    Semi,
    Amp,
    As
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub line: usize,
}

pub struct Tokenizer<'a> {
    pub source: &'a str,
    pub tokens: Vec<Token>,
    pub cursor: usize,
    pub line: usize,
}

pub fn get_type(t: String) -> Option<TokenKind> {
    match t.as_str() {
        "u8" => Some(TokenKind::U8),
        "u16" => Some(TokenKind::U16),
        "u32" => Some(TokenKind::U32),
        "u64" => Some(TokenKind::U64),
        "i8" => Some(TokenKind::I8),
        "i16" => Some(TokenKind::I16),
        "i32" => Some(TokenKind::I32),
        "i64" => Some(TokenKind::I64),
        "f32" => Some(TokenKind::F32),
        "f64" => Some(TokenKind::F64),
        "byte" => Some(TokenKind::Byte),
        "bool" => Some(TokenKind::Bool),
        "void" => Some(TokenKind::Void),
        _ => None
    }
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            source,
            tokens: Vec::new(),
            cursor: 0,
            line: 1
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

        0 as char
    }

    fn push_token(&mut self, kind: TokenKind, value: String) {
        self.tokens.push(Token{line: self.line, kind, value})
    }

    fn is_symbol(&mut self) -> bool {
        match self.peek() {
            '(' | ')' | '{' | '}' | '+' | '-' | '*' | '/' | '=' | ',' | ';' | '&' => true,
            _ => false
        }
    }

    pub fn tokenize(&mut self) {
        while self.ok() {
            let c = self.peek();

            if c.is_whitespace() {
                if c == '\n' {
                    self.line += 1;
                }

                self.cursor += 1;
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

        while self.ok() && (self.peek().is_alphanumeric() || self.peek() == '_') {
            ident.push(self.peek());
            self.cursor += 1;
        }

        if let Some(keyword) = self.get_keyword(ident.clone()) {
            return self.push_token(keyword, ident);
        }

        if let Some(t) = get_type(ident.clone()) {
            return self.push_token(t, ident);
        }

        self.push_token(TokenKind::Identifier, ident);
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
            "ret" => Some(TokenKind::Return),
            "as" => Some(TokenKind::As),
            "true" => Some(TokenKind::TrueLiteral),
            "false" => Some(TokenKind::FalseLiteral),
            _ => None
        }
    }

    fn consume_symbol(&mut self) {
        match self.peek() {
            '(' => self.push_token(TokenKind::OpenParen, '('.to_string()),
            ')' => self.push_token(TokenKind::CloseParen, ')'.to_string()),
            '{' => self.push_token(TokenKind::OpenCurly, '{'.to_string()),
            '}' => self.push_token(TokenKind::CloseCurly, '}'.to_string()),
            '+' => self.push_token(TokenKind::Add, '+'.to_string()),
            '-' => self.push_token(TokenKind::Subtract, '-'.to_string()),
            '*' => self.push_token(TokenKind::Multiply, '*'.to_string()),
            '/' => self.push_token(TokenKind::Divide, '/'.to_string()),
            '=' => self.push_token(TokenKind::Equals, '='.to_string()),
            ',' => self.push_token(TokenKind::Comma, ','.to_string()),
            ';' => self.push_token(TokenKind::Semi, ';'.to_string()),
            '&' => self.push_token(TokenKind::Amp, '&'.to_string()),
            tok => println!("unknown token: {}", tok)
        }

        self.cursor += 1;
    }
}