use crate::lexer::Lexer;
use std::fs;
use crate::io::info;


pub fn parse(file: String) {
    let code = fs::read_to_string(file.clone())
        .expect("Something went wrong reading the file");

    info(format!("The code contains:\n  {}", code).as_str());

    let mut lexer = Lexer::new();

    lexer.file = Some(file);
    for character in code.chars() {
        lexer.lex(character);
    }

    lexer.finish();
    lexer.pretty_print_tokens();
}
