use std::{env, fs};
use crate::io::{info, error};
use crate::lexer::Lexer;
use crate::ast::AST;
use crate::bytecode::BytecodeGenerator;

mod lexer;
mod io;
mod token;
mod ast;
mod bytecode;
mod llvm;

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
    let code_block = match ast.parse_ast(lexer.tokens) {
        Ok(code_block) => code_block,
        Err(e) => panic!(e),
    };
    
    let bytecode = BytecodeGenerator::generate(&code_block);
    llvm::compile(bytecode);
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
