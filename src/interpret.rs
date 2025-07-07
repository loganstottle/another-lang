use crate::{ast::{Block, Expr, Stmt, Value}, interpret};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub struct Declared {
    pub name: String,
    pub args: Vec<String>,
    pub body: Block
}

#[derive(Debug, PartialEq, Clone)]
pub struct Builtin {
    pub name: String,
    pub body: fn(Vec<Value>) -> Value
}

#[derive(Debug, PartialEq, Clone)]
pub enum Function {
    Declared(Declared),
    Builtin(Builtin)
}

pub struct Context {
    pub variables: HashMap<String, Value>,
    pub functions: HashMap<String, Function>
}

impl Function {
    pub fn call(&mut self, mut context: &mut Context, args: Vec<Expr>) -> Value {
        match self {
            Function::Declared(func) => {
                let mut ctx = Context::new();

                ctx.variables = context.variables.clone();
                ctx.functions = context.functions.clone();

                for (name, arg) in func.args.iter().zip(args) {
                    ctx.variables.insert(name.clone(), arg.eval(&mut context));
                }

                let mut val = Value::Null;

                for stmt in func.body.clone() {
                    match stmt {
                        Stmt::Return(expr) => return expr.eval(&mut ctx),
                        _ => val = stmt.run(&mut ctx)
                    };
                }

                val
            },
            Function::Builtin(func) => {
                (func.body)(args.clone().iter_mut().map(|arg| arg.to_owned().eval(&mut context)).collect())
            }
        }
    }
}

fn log(vals: Vec<Value>) -> Value {
    let mut output = String::new();

    for (i, val) in vals.into_iter().enumerate() {
        let str = match val {
            Value::Number(n) => n.to_string(),
            Value::String(s) => s,
            Value::Boolean(b) => b.to_string(),
            Value::List(_) => String::from("LIST"),
            Value::Function(func) => {
                match func {
                    interpret::Function::Declared(declared) => {
                        format!("fn {}({}) {{ ... }}", declared.name, declared.args.join(", "))
                    },
                    interpret::Function::Builtin(builtin) => {
                        format!("fn {}(...) {{ builtin }}", builtin.name)
                    },
                }
            },
            Value::Null => String::from("NULL"),
        };

        if i > 0 {
            output += ", ";
        }

        output += &str;
    }

    println!("{}", output);

    Value::Null
}

fn get_builtins() -> HashMap<String, Function> {
    let mut functions: HashMap::<String, Function> = HashMap::new();

    functions.insert(String::from("log"), Function::Builtin(Builtin { name: String::from("log"), body: log }));

    functions
}

impl Context {
    pub fn new() -> Context {
        Context { variables: HashMap::new(), functions: get_builtins() }
    }
}