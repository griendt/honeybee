use crate::lexer::Lexer;
use std::fs;
use crate::io::info;
use crate::ast::AST;


pub fn parse(file: String) {
    let code = fs::read_to_string(file.clone())
        .expect("Something went wrong reading the file");

    info("Source code:");
    println!("  {}", code);

    let mut lexer = Lexer::new();

    lexer.file = Option::from(file);
    for character in code.chars() {
        lexer.lex(character);
    }

    lexer.finish();

    info("Lexer result:");
    lexer.pretty_print_tokens();

    let ast = AST::new();
    ast.make(lexer.tokens.as_mut());

    info("Pre-AST parsing result:");
    lexer.pretty_print_tokens();
}
