use crate::token::{Token, TokenKind, get_type};

#[derive(Debug, PartialEq, Clone)]
pub struct Signature {
    pub name: String,
    pub args: Vec<Arg>,
    pub return_type: Type
}

#[derive(Debug, PartialEq, Clone)]
pub struct Arg {
    pub name: String,
    pub typ: Type
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub name: String,
    pub args: Vec<String>,
    pub body: Block
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Float(f64),
    Integer(i64),
    String(String),
    Boolean(bool),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    U8, U16, U32, U64,
    I8, I16, I32, I64, 
    F32, F64,
    Char,
    Boolean,
    Pointer(Box<Type>),
    Void,
    Invalid
}

fn convert_binop(kind: TokenKind) -> Option<BinOp> {
    match kind {
        TokenKind::Add => Some(BinOp::Add),
        TokenKind::Subtract => Some(BinOp::Subtract),
        TokenKind::Multiply => Some(BinOp::Multiply),
        TokenKind::Divide => Some(BinOp::Divide),
        _ => None
    }
}

impl BinOp {
    fn prec(&mut self) -> usize {
        match self {
            BinOp::Add => 2,
            BinOp::Subtract => 2,
            BinOp::Multiply => 1,
            BinOp::Divide => 1,
        }
    }

    fn assoc(&mut self) -> usize {
        match self {
            BinOp::Add => 0,
            BinOp::Subtract => 0,
            BinOp::Multiply => 0,
            BinOp::Divide => 0,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Literal(Literal),
    Variable(String),
    Binary {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>
    },
    FnCall(String, Vec<Expr>),
    Reference(Box<Expr>),
    Dereference(Box<Expr>),
    Cast(Box<Expr>, Type),
}

pub type Block = Vec<Stmt>;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expr(Expr),
    Return(Expr),
    VarDeclare(Type, String, Expr),
    VarAssign(String, Expr),
    FnDefine(Signature, Block)
}

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
    pub program: Vec<Stmt>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            cursor: 0,
            program: Vec::new(),
        }
    }

    fn ok(&self) -> bool {
        self.inbounds(0)
    }

    fn inbounds(&self, offset: usize) -> bool {
        (self.cursor + offset) < self.tokens.len()
    }

    fn peek(&self) -> Token {
        self.tokens[self.cursor].clone()
    }

    fn expect(&self, kind: TokenKind) {
        if !self.ok() {
            panic!("(VM:{}) expected {:?}, found EOF",self.peek().line, kind);
        }

        if self.peek().kind != kind {
            panic!("(VM:{}) expected {:?}, found {:?}", self.peek().line, kind, self.peek().kind);
        }
    }

    fn parse_fn_call(&mut self, name: String) -> Expr {
        self.cursor += 1;

        let mut args: Vec<Expr> = Vec::new();

        if self.peek().kind == TokenKind::CloseParen {
            self.cursor += 1;
            return Expr::FnCall(name, args);
        }

        while self.peek().kind != TokenKind::CloseParen {
            args.push(self.parse_expr());

            if self.peek().kind == TokenKind::CloseParen {
                self.cursor += 1;
                break
            }

            self.expect(TokenKind::Comma);
            self.cursor += 1;
        }

        Expr::FnCall(name, args)
    }

    fn convert_type(&mut self) -> Option<Type> {
        if let Some(t) = get_type(self.peek().value) {
            let mut typ = match t {
                TokenKind::U8 => Type::U8,
                TokenKind::U16 => Type::U16,
                TokenKind::U32 => Type::U32,
                TokenKind::U64 => Type::U64,
                TokenKind::I8 => Type::I8,
                TokenKind::I16 => Type::I16,
                TokenKind::I32 => Type::I32,
                TokenKind::I64 => Type::I64,
                TokenKind::F32 => Type::F32,
                TokenKind::F64 => Type::F64,
                TokenKind::Byte => Type::Char,
                TokenKind::Bool => Type::Boolean,
                TokenKind::Void => Type::Void,
                _ => panic!("{} is not type", self.peek().value)
            };

            self.cursor += 1;

            while self.peek().kind == TokenKind::Multiply {
                typ = Type::Pointer(Box::new(typ));
                self.cursor += 1;
            }

            return Some(typ)
        }

        None
    }

    fn parse_block(&mut self) -> Block {
        let mut block: Block = Vec::new();

        self.expect(TokenKind::OpenCurly);
        self.cursor += 1;

        while self.peek().kind != TokenKind::CloseCurly {
            let stmt = self.parse_statement();

            block.push(stmt.clone());

            if let Stmt::FnDefine(_, _) = stmt {
            } else {
                self.expect(TokenKind::Semi);
                self.cursor += 1;
            }
        }

        self.cursor += 1;

        block
    }

    fn parse_primary(&mut self) -> Expr {
        let next = self.peek();
        self.cursor += 1;

        let expr = match next.kind {
            TokenKind::NumberLiteral => {
                let typ = if next.value.contains(".") {
                    Literal::Float(next.value.parse::<f64>().unwrap())
                } else {
                    Literal::Integer(next.value.parse::<i64>().unwrap())
                };

                Expr::Literal(typ)
            },
            TokenKind::StringLiteral => Expr::Literal(Literal::String(next.value)),
            TokenKind::TrueLiteral => Expr::Literal(Literal::Boolean(true)),
            TokenKind::FalseLiteral => Expr::Literal(Literal::Boolean(false)),
            TokenKind::OpenParen => {
                let expr = self.parse_expr();

                self.expect(TokenKind::CloseParen);
                self.cursor += 1;

                expr
            },
            TokenKind::Identifier => {
                if self.ok() && self.peek().kind == TokenKind::OpenParen {
                    return self.parse_fn_call(next.value)
                }

                Expr::Variable(next.value)
            },
            _ => panic!("(VM:{}) expected expression, found {:?}", next.line, next.kind)
        };

        if self.peek().kind == TokenKind::As {
            self.cursor += 1;

            if let Some(t) = self.convert_type() {
                Expr::Cast(Box::new(expr), t)
            } else {
                panic!("expected type following \"as\"")
            }
        } else {
            expr
        }
    }

    fn parse_unary(&mut self) -> Expr {
        let token = self.peek();

        match token.kind {
            TokenKind::Multiply => {
                self.cursor += 1;
                Expr::Dereference(Box::new(self.parse_unary()))
            }
            TokenKind::Amp => {
                self.cursor += 1;
                Expr::Reference(Box::new(self.parse_unary()))
            }
            _ => self.parse_primary()
        }
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_expr_prec(usize::MAX)
    }

    fn parse_expr_prec(&mut self, min_prec: usize) -> Expr {
        let mut lhs = self.parse_unary();

        while self.ok() {
            if let Some(b) = &mut convert_binop(self.peek().kind) {
                if b.prec() > min_prec {
                    break;
                }

                let next_min_prec = if b.assoc() == 0 {
                    b.prec() - 1
                } else {
                    b.prec()
                };

                let operator = match self.peek().kind {
                    TokenKind::Add => BinOp::Add,
                    TokenKind::Subtract => BinOp::Subtract,
                    TokenKind::Multiply => BinOp::Multiply,
                    TokenKind::Divide => BinOp::Divide,
                    _ => panic!("expected operator at {:?}", self.peek().kind)
                };

                self.cursor += 1;

                lhs = Expr::Binary { op: operator, left: Box::new(lhs), right: Box::new(self.parse_expr_prec(next_min_prec)) };
            } else {
                break
            }
        }

        lhs
    }

    pub fn parse_statement(&mut self) -> Stmt {
        let tok = self.peek();

        if let Some(t) = self.convert_type() {
            self.expect(TokenKind::Identifier);

            let name = self.peek().value;
            self.cursor += 1;

            self.expect(TokenKind::Equals);
            self.cursor += 1;

            return Stmt::VarDeclare(t, name, self.parse_expr())
        } else {
            match tok.kind {
                TokenKind::Identifier => {
                    if self.inbounds(2) && self.tokens[self.cursor+1].kind == TokenKind::Equals {
                        self.cursor += 2;
                        return Stmt::VarAssign(tok.value, self.parse_expr())
                    } else if self.tokens[self.cursor+1].kind == TokenKind::OpenParen {
                        self.cursor += 1;
                        return Stmt::Expr(self.parse_fn_call(tok.value))
                    }
                },
                TokenKind::Return => {
                    self.cursor += 1;

                    return Stmt::Return(self.parse_expr())
                },
                TokenKind::Fn => {
                    self.cursor += 1;

                    self.expect(TokenKind::Identifier);

                    let name = self.peek().value;
                    self.cursor += 1;

                    self.expect(TokenKind::OpenParen);
                    self.cursor += 1;

                    let mut args: Vec<Arg> = vec![];

                    while self.peek().kind != TokenKind::CloseParen {
                        if let Some(t) = self.convert_type() {
                            self.expect(TokenKind::Identifier);

                            args.push(Arg{typ: t, name: self.peek().value});
                            self.cursor += 1;

                            if self.peek().kind == TokenKind::Comma {
                                self.cursor += 1;
                            }
                        } else {
                            panic!("expected type")
                        }
                    }

                    self.cursor += 1;

                    let t = if let Some(t) = self.convert_type() {
                        t
                    } else if self.peek().kind == TokenKind::OpenCurly {
                        Type::Void
                    } else {
                        panic!("expected type or block")
                    };

                    return Stmt::FnDefine(Signature { name, args: args.clone(), return_type: t }, self.parse_block())
                },
                _ => ()
            }
        }

        Stmt::Expr(self.parse_expr())
    }

    pub fn parse(&mut self) {
        while self.ok() {
            let stmt = self.parse_statement();
            self.program.push(stmt.clone());

            if let Stmt::FnDefine(_, _) = stmt {
            } else {
                self.expect(TokenKind::Semi);
                self.cursor += 1;
            }
        }
    }

    pub fn to_c(&mut self) -> String {
        let mut output = String::new();

        for stmt in &mut self.program {
            output += format!("{}\n", stmt.to_c()).as_str();

            if let Stmt::FnDefine(_, _) = stmt {
                output += "\n";
            }
        }

        output
    }
}