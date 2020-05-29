#![feature(core_panic)]

use std::{env, fs};
use crate::io::{info, error};
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

    info("Finished lexing");
    // info("Lexer result:");
    // lexer.pretty_print_tokens();

    let mut ast = AST::new();
    lexer.tokens.iter_mut().for_each(|token|
        token._type = Option::from(ast.parse_token(token)
            .expect(format!("Syntax error when parsing token {:#?}", token).as_str())
        )
    );

    info("Finished token parsing");
    // info("Token parse result was:");
    // lexer.pretty_print_tokens();

    info("Generating AST...");
    match ast.build_and_run(lexer.tokens) {
        Ok(_) => {
            info("Global state after execution:");
            println!("{:?}", ast.scope);
        },
        Err(err) => {
            error(format!("  {}\n  Execution failed.", err).as_str());
            info("Dump of state:");
            println!("  {:?}", ast.scope);
        }
    }
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
