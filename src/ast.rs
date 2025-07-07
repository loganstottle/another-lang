use crate::token::{Token, TokenKind};
use crate::interpret::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    List(Vec<Value>),
    Function(Function),
    Null,
}

impl Value {
    pub fn string(self) -> String {
        match self {
            Value::Number(n) => n.to_string(),
            Value::String(s) => s,
            Value::Boolean(b) => String::from("bool"),
            Value::List(l) => String::from("list"),
            Value::Function(f) => String::from("function"),
            Value::Null => String::from("NULL"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Literal(Value),
    Variable(String),
    Binary {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>
    },
    FnCall(Box<Expr>, Vec<Expr>),
    FnDefine(String, Vec<String>, Block)
}

pub type Block = Vec<Stmt>;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expr(Expr),
    Return(Expr),
    Assignment(String, Expr)
}

impl Expr {
    pub fn eval(self, context: &mut Context) -> Value {
        match self {
            Expr::Literal(val) => val.clone(),
            Expr::Variable(name) => {
                if let Some(value) = context.variables.get(&name) {
                    return value.clone()
                }

                if let Some(value) = context.functions.get(&name) {
                    return Value::Function(value.clone())
                }
            
                println!("variable not found: {}", name);

                Value::Null
            },
            Expr::Binary {op, left, right } => {
                let lhs = left.eval(context);
                let rhs = right.eval(context);

                match (lhs, rhs) {
                    (Value::String(s1), Value::String(s2)) => Value::String(s1 + s2.as_str()),
                    (Value::Number(l), Value::Number(r)) => Value::Number(
                        match op {
                            BinOp::Add => l + r,
                            BinOp::Subtract => l - r,
                            BinOp::Multiply => l * r,
                            BinOp::Divide => l / r
                        }
                    ),
                    _ => panic!("wrong operand types")
                }
            },
            Expr::FnDefine(name, args, body) => {
                let func = Function::Declared(Declared { name: name.clone(), args: args.clone(), body: body.clone() });
                
                context.functions.insert(name, func.clone());
                
                Value::Function(func)
            },
            Expr::FnCall(func, args) => {
                match *func {
                    Expr::Variable(name) => {
                        if let Some(mutfunc) = context.functions.get_mut(&name) { 
                            return mutfunc.clone().call(context, args)
                        } else {
                            panic!("function not defined")
                        }
                    },
                    Expr::FnDefine(_, names, body) => {
                        let mut ctx = Context::new();

                        ctx.variables = context.variables.clone();
                        ctx.functions = context.functions.clone();

                        for (name, arg) in names.iter().zip(args) {
                            ctx.variables.insert(name.clone(), arg.eval(context));
                        }

                        let mut val = Value::Null;

                        for stmt in body {
                            match stmt {
                                Stmt::Return(expr) => return expr.eval(&mut ctx),
                                _ => val = stmt.run(&mut ctx)
                            };
                        }

                        val
                    },
                    _ => panic!("invalid function call")
                }
            }
        }
    }
}

impl Stmt {
    pub fn run(self, context: &mut Context) -> Value {
        match self {
            Stmt::Assignment(name, expr) => {
                let value = expr.eval(context);

                context.variables.insert(name.clone(), value.clone());
                return value;
            },
            Stmt::Return(expr) => expr.eval(context),
            Stmt::Expr(expr) => expr.eval(context),
        }
    }
}

pub struct Parser<'a> {
    tokens: Vec<Token>,
    cursor: usize,
    pub program: Vec<Stmt>,
    context: &'a mut Context,
}

impl<'a> Parser<'a> {
    pub fn new(context: &'a mut Context, tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens,
            cursor: 0,
            program: Vec::new(),
            context
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

    fn parse_term(&mut self) -> Expr {
        let factor = self.parse_factor();
        
        if !self.ok() {
            return factor
        }

        let operator = match self.peek().kind {
            TokenKind::Multiply => BinOp::Multiply,
            TokenKind::Divide => BinOp::Divide,
            _ => return factor
        };
        
        self.cursor += 1;

        Expr::Binary{op: operator, left: Box::new(factor), right: Box::new(self.parse_term()) }
    }

    fn parse_fn_call(&mut self, func: Expr) -> Expr {
        self.cursor += 1;

        let mut args: Vec<Expr> = Vec::new();
        
        if self.peek().kind == TokenKind::CloseParen {
            self.cursor += 1;
            return Expr::FnCall(Box::new(func), args);
        }

        while self.peek().kind != TokenKind::CloseParen {
            args.push(self.parse_expr());
            
            if self.peek().kind == TokenKind::CloseParen {
                self.cursor += 1;
                break
            }

            assert!(self.peek().kind == TokenKind::Comma);
            self.cursor += 1;
        }

        return Expr::FnCall(Box::new(func), args);
    }

    fn parse_factor(&mut self) -> Expr {
        let next = self.peek();
        self.cursor += 1;

        match next.kind {
            TokenKind::NumberLiteral => Expr::Literal(Value::Number(next.value.parse().unwrap())),
            TokenKind::StringLiteral => Expr::Literal(Value::String(next.value)),
            TokenKind::OpenParen => {
                let expr = self.parse_expr();

                assert!(self.peek().kind == TokenKind::CloseParen);
                self.cursor += 1;

                if self.ok() && self.peek().kind == TokenKind::OpenParen {
                    return self.parse_fn_call(expr)
                }

                expr
            },
            TokenKind::Variable => {
                let func = Expr::Variable(next.value);

                if self.ok() && self.peek().kind == TokenKind::OpenParen {
                    return self.parse_fn_call(func)
                }

                func
            },
            TokenKind::Fn => {
                let mut name = String::new();

                match self.peek().kind {
                    TokenKind::Variable => {
                        name = self.peek().value;
                        self.cursor += 1;
                    },
                    TokenKind::OpenParen => (),
                    _ => panic!("bad token while looking for function name")
                }

                assert!(self.peek().kind == TokenKind::OpenParen);
                self.cursor += 1;

                let mut args: Vec<String> = Vec::new();

                while self.peek().kind != TokenKind::CloseParen {
                    args.push(self.peek().value);

                    self.cursor += 1;

                    if self.peek().kind == TokenKind::CloseParen {
                        break
                    }

                    assert!(self.peek().kind == TokenKind::Comma);
                    self.cursor += 1;
                }

                self.cursor += 1;
                
                Expr::FnDefine(name, args, vec![self.parse_statement()])
            },
            _ => panic!("invalid expression at {:?}", next.kind)
        }
    }

    fn parse_expr(&mut self) -> Expr {
        let mut expr = self.parse_term();
        
        while self.ok() {
            let operator = match self.peek().kind {
                TokenKind::Add => BinOp::Add,
                TokenKind::Subtract => BinOp::Subtract,
                _ => return expr
            };
    
            self.cursor += 1;
            
            expr = Expr::Binary { op: operator, left: Box::new(expr), right: Box::new(self.parse_term()) };
        }

        expr
    }

    pub fn parse_statement(&mut self) -> Stmt {
        let tok = self.peek();

        if tok.kind == TokenKind::Variable &&
        self.inbounds(1) && 
        self.tokens[self.cursor+1].kind == TokenKind::Equals {
            self.cursor += 2;

            let mut expr = self.parse_expr();

            if let Expr::FnDefine(_, args, body) = expr {
                expr = Expr::FnDefine(tok.value.clone(), args, body)
            }

            return Stmt::Assignment(tok.value, expr)
        } else if tok.kind == TokenKind::Return {
            self.cursor += 1;

            return Stmt::Return(self.parse_expr())
        }

        Stmt::Expr(self.parse_expr())
    }

    pub fn parse(&mut self) {
        while self.ok() {
            let stmt = self.parse_statement();
            self.program.push(stmt);
        }
    }

    pub fn eval(&mut self) {
        for stmt in self.program.clone() { 
            stmt.run(self.context);
        };
    }
}