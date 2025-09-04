use std::fs;

mod token;
mod ast;
mod cgen;
mod semantic;

fn main() {
    let contents = match fs::read_to_string("main.dk") {
        Ok(contents) => contents,
        Err(e) => panic!("could not read src file {}", e)
    };

    let mut tokenizer = token::Tokenizer::new(contents.as_str());
    tokenizer.tokenize();

    println!("Tokens:");

    for token in &tokenizer.tokens {
        println!("{:?}", token);
    }

    let mut parser = ast::Parser::new(tokenizer.tokens);
    parser.parse();

    dbg!(parser.program.clone());
    println!("\n\nC: \n\n{}", parser.to_c());

    let mut s = semantic::SemanticAnalyzer::new(parser.program);
    s.analyze();
}
