use crate::ast::*;

impl Type {
    pub fn to_c(&mut self) -> String {
        match self {
            Type::U8 => String::from("uint8_t"),
            Type::U16 => String::from("uint16_t"),
            Type::U32 => String::from("uint32_t"),
            Type::U64 => String::from("uint64_t"),
            Type::I8 => String::from("int8_t"),
            Type::I16 => String::from("int16_t"),
            Type::I32 => String::from("int32_t"),
            Type::I64 => String::from("int64_t"),
            Type::F32 => String::from("float"),
            Type::F64 => String::from("double"),
            Type::Char => String::from("char"),
            Type::Boolean => String::from("bool"),
            Type::Void => String::from("void"),
            Type::Invalid => String::from("invalid"),
            Type::Pointer(t) => format!("{}*", t.to_c()),
        }
    }
}

impl BinOp {
    pub fn to_c(&mut self) -> String {
        match self {
            BinOp::Add => String::from("+"),
            BinOp::Subtract => String::from("-"),
            BinOp::Multiply => String::from("*"),
            BinOp::Divide => String::from("/"),
        }
    }
}

impl Literal {
    fn to_c(&mut self) -> String {
        match self {
            Literal::Float(num) => num.to_string(),
            Literal::Integer(num) => num.to_string(),
            Literal::String(str) => format!("\"{}\"", str.clone()),
            Literal::Boolean(val) => format!("{}", val),
            }
    }
}

impl Expr {
    fn to_c(&mut self) -> String {
        match self {
            Expr::Literal(value) => value.to_c(),
            Expr::Variable(name) => name.clone(),
            Expr::Binary { op, left, right } => format!("({}) {} ({})", left.to_c(), op.to_c(), right.to_c()),
            Expr::FnCall(name, args) => format!("{}({})", name, args.iter_mut().map(|arg| arg.to_c()).collect::<Vec<String>>().join(", ")),
            Expr::Dereference(expr) => format!("*({})", expr.to_c()),
            Expr::Reference(expr) => format!("&{}", expr.to_c()),
            Expr::Cast(expr, typ ) => format!("({})({})", typ.to_c(), expr.to_c()),
        }
    }
}

impl Stmt {
    pub fn to_c(&mut self) -> String {
        match self {
            Stmt::Expr(value) => format!("{};", value.to_c()),
            Stmt::Return(value) => format!("return {};", value.to_c()),
            Stmt::VarAssign(name, value) => format!("{} = {};", name, value.to_c()),
            Stmt::VarDeclare(typ, name, value) => format!("{} {} = {};", typ.to_c(), name, value.to_c()),
            Stmt::FnDefine(Signature { name, args, return_type }, body) => {
                let args_str = args.iter_mut().map(|arg| format!("{} {}", arg.typ.to_c(), arg.name)).collect::<Vec<String>>().join(", ");
                let body_str = body.iter_mut().map(|stmt| stmt.to_c().split("\n").collect::<Vec<&str>>().join("\n  ")).collect::<Vec<String>>().join("\n  ");

                format!("\n{} {}({}) {{\n  {}\n}}\n", return_type.to_c(), name, args_str, body_str)
            }
        }
    }
}
