use std::{fs, io};

mod token;
mod ast;
mod interpret;

fn repl() {
    let stdin = io::stdin();
    let mut input = String::new();

    let mut ctx = interpret::Context::new();
    
    loop {
        stdin.read_line(&mut input).unwrap();
        
        let mut tokenizer = token::Tokenizer::new(input.as_str());
        tokenizer.tokenize();
    
        let mut parser = ast::Parser::new(&mut ctx, tokenizer.tokens);
        parser.parse();

        println!("return: {}", parser.program[0].clone().run(&mut ctx).string());

        input = String::new();
    }
}

fn main() {
    // let contents = match fs::read_to_string("./src.txt") {
    //     Ok(contents) => contents,
    //     Err(e) => panic!("could not read src file {}", e)
    // };

    // let ctx = &mut interpret::Context::new();

    // let mut tokenizer = token::Tokenizer::new(contents.as_str());
    // tokenizer.tokenize();

    // let mut parser = ast::Parser::new(ctx, tokenizer.tokens);
    // parser.parse();
    // parser.eval();

    repl();
}
