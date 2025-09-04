use std::collections::HashMap;
use crate::ast::*;

impl Type {
    fn numeric(self) -> bool {
        match self {
            Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::F32 | Type::F64 => true,
            _ => false,
        }
    }

    fn integer(self) -> bool {
        match self {
            Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::I8 | Type::I16 | Type::I32 | Type::I64 => true,
            _ => false,
        }
    }

    fn pointer(self) -> bool {
        match self {
            Type::Pointer(_) => true,
            _ => false,
        }
    }
}

pub struct Scope {
    variables: HashMap<String, Type>,
    functions: HashMap<String, Signature>,
}

pub struct SemanticAnalyzer {
    pub program: Vec<Stmt>,
    pub scopes: Vec<Scope>,
    pub function_stack: Vec<Signature>,
    pub errors: Vec<String>,
}

impl SemanticAnalyzer {
    pub fn new(program: Vec<Stmt>) -> SemanticAnalyzer {
        SemanticAnalyzer { program, scopes: vec![Scope { variables: HashMap::new(), functions: HashMap::new() }], function_stack: Vec::new(), errors: Vec::new() }
    }

    fn store_variable(&mut self, name: String, typ: Type) {
        self.scopes.last_mut().unwrap().variables.insert(name, typ);
    }

    fn get_variable(&mut self, name: String) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if scope.variables.contains_key(&name) {
                return scope.variables.get(&name).cloned();
            }
        }

        None
    }

    fn store_function(&mut self, signature: Signature) {
        self.scopes.last_mut().unwrap().functions.insert(signature.name.clone(), signature);
    }

    fn get_function(&mut self, name: String) -> Option<Signature> {
        for scope in self.scopes.iter().rev() {
            if scope.functions.contains_key(&name) {
                return scope.functions.get(&name).cloned();
            }
        }

        None
    }

    pub fn error(&mut self, msg: String) {
        self.errors.push(msg);
    }

    pub fn error_dump(&self) {
        println!("Error: {}", self.errors.iter().map(|err| err.to_lowercase()).collect::<Vec<String>>().join("\nError: "));
    }

    fn enter_function(&mut self, signature: Signature) {
        self.function_stack.push(signature.clone());

        let mut scope = Scope { variables: HashMap::new(), functions: HashMap::new() };

        for arg in signature.args.clone() {
            scope.variables.insert(arg.name.clone(), arg.typ.clone());
        }

        self.scopes.push(scope);
    }

    fn exit_function(&mut self) {
        self.function_stack.pop();
        self.scopes.pop();
    }

    fn analyze_return(&mut self, expr: Expr) {
        if self.function_stack.len() == 0 {
            self.error(String::from("cannot return from outside of a function body"));
            return;
        }

        let expected_return_type = self.function_stack.last().unwrap().clone().return_type;
        let got_return_type = self.analyze_expr(expr);

        if expected_return_type != got_return_type {
            self.error(format!("expected {:?}, got {:?}", expected_return_type, got_return_type));
        }
    }

    fn analyze_expr(&mut self, expr: Expr) -> Type {
        match expr {
            Expr::Literal(value) => {
                match value {
                    Literal::Float(_) => Type::F32,
                    Literal::Integer(_) => Type::I32,
                    Literal::String(_) => Type::Pointer(Box::new(Type::Char)),
                    Literal::Boolean(_) => Type::Boolean,
                }
            },
            Expr::Variable(name) => {
                if let Some(t) = self.get_variable(name.clone()) {
                    return t
                }

                self.error(format!("\"{}\" is not defined", name));
                Type::Invalid
            },
            Expr::Binary { op, left, right } => {
                let type1 = &mut self.analyze_expr(*left);
                let type2 = &mut self.analyze_expr(*right);

                if type1.clone().integer() && type2.clone().integer() {
                    return Type::I32
                } else if type1.clone().numeric() && type2.clone().numeric() {
                    return Type::F32
                }

                if (op == BinOp::Add || op == BinOp::Subtract) {
                    if type1.clone().pointer() && type2.clone().integer() {
                        return type1.clone()
                    } else if op == BinOp::Add && type1.clone().integer() && type2.clone().pointer() {
                        return type2.clone()
                    }
                }

                match (type1.clone(), type2.clone()) {
                    (_, Type::Invalid) | (Type::Invalid, _) => Type::Invalid,
                    (_, _) => {
                        self.error(format!("operator {:?} invalid between {:?} and {:?}", op, type1, type2));
                        Type::Invalid
                    },
                }
            },
            Expr::FnCall(name, _) => {
                if self.get_function(name.clone()) != None {
                    return Type::Invalid
                }

                self.error(format!("{} is not a function", name));
                Type::Invalid
            },
            Expr::Dereference(expr) => {
                let typ = self.analyze_expr(*expr.clone());
                if typ == Type::Invalid {
                    return Type::Invalid;
                }

                match typ {
                    Type::Pointer(ptr) => *ptr,
                    _ => {
                        self.error(format!("cannot dereference non pointer \"{:?}\"", typ));
                        Type::Invalid
                    }
                }
            },
            Expr::Reference(expr) => {
                let typ = self.analyze_expr(*expr.clone());
                if typ == Type::Invalid {
                    return Type::Invalid;
                }

                match *expr {
                    Expr::Variable(_) => (),
                    _ => {
                        self.error(String::from("cannot reference non-lvalue"));
                        return Type::Invalid;
                    }
                };

                Type::Pointer(Box::new(typ))
            },
            Expr::Cast(_, typ) => typ
        }
    }

    fn analyze_var_declare(&mut self, typ: Type, name: String, expr: Expr) {
        if typ == Type::Void {
            self.error(String::from("cannot declare void variable"));
            return
        }

        if self.scopes.last().unwrap().variables.get(&name).cloned() != None {
            self.error(format!("\"{}\" already declared", name));
            return;
        }

        let got_typ = self.analyze_expr(expr);

        if got_typ == Type::Invalid {
            return;
        }

        let numeric_cast = typ.clone().numeric() && got_typ.clone().numeric();
        let pointer_cast = typ.clone().pointer() && got_typ.clone().pointer();

        if !numeric_cast && !pointer_cast && typ != got_typ {
            self.error(format!("expected {:?}, got {:?}", typ, got_typ));
            return;
        }

        self.store_variable(name, typ);
    }

    fn analyze_var_assign(&mut self, name: String, expr: Expr) {
        let got_typ = self.analyze_expr(expr);
        if got_typ == Type::Invalid {
            return;
        }

        if let Some(typ) = self.get_variable(name.clone()) {
            let numeric_cast = typ.clone().numeric() && got_typ.clone().numeric();
            let pointer_cast = typ.clone().pointer() && got_typ.clone().pointer();

            if !numeric_cast && !pointer_cast && typ != got_typ  {
                self.error(format!("expected {:?}, got {:?}", typ, got_typ));
            }
        } else {
            self.error(format!("\"{}\" is not defined", name));
        }
    }

    fn analyze_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Expr(expr) => { self.analyze_expr(expr); },
            Stmt::Return(expr) => self.analyze_return(expr),
            Stmt::VarAssign(name, expr) => self.analyze_var_assign(name, expr),
            Stmt::VarDeclare(typ, name, expr) => self.analyze_var_declare(typ, name, expr),
            Stmt::FnDefine(signature, body) => {
                self.enter_function(signature.clone());

                for stmt in body {
                    self.analyze_stmt(stmt);
                }

                self.exit_function();

                self.store_function(signature);
            }
        }
    }

    pub fn analyze(&mut self) {
        for stmt in self.program.clone() {
            self.analyze_stmt(stmt.clone());
        }

        if self.errors.len() > 0 {
            self.error_dump();
        }
    }
}