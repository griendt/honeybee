#![feature(core_panic)]

use std::{env, fs};
use crate::io::info;
use crate::lexer::Lexer;
use crate::ast::AST;

mod lexer;
mod io;
mod token;
mod ast;

fn run(file: String) {

    let code = fs::read_to_string(file.clone())
        .expect("Something went wrong reading the file");

    info("Source code:");
    println!("  {}", code.replace("\n", "\n  "));

    let mut lexer = Lexer::new();

    lexer.file = Option::from(file);
    for character in code.chars() {
        lexer.lex(character);
    }

    lexer.finish();

    info("Lexer result:");
    lexer.pretty_print_tokens();

    let mut ast = AST::new();
    lexer.tokens.iter_mut().for_each(|token| ast.parse_token(token));

    info("Token parse result was:");
    lexer.pretty_print_tokens();

    info("Generating AST...");
    ast.build_and_run(lexer.tokens);

    info("Global state after execution:");
    println!("{:?}", ast.scope)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut first_arg = true;

    for arg in args {
        if first_arg {
            // The first argument is the executable itself
            first_arg = false;
            continue;
        }

        run(arg);
    }

}
