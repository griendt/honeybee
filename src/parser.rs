use crate::lexer::Lexer;
use std::fs;


pub fn parse(file: String) {
    let code = fs::read_to_string(file.clone())
        .expect("Something went wrong reading the file");

    println!("The code contains:\n  {}", code);

    let mut lexer = Lexer::new();

    lexer.file = Some(file);
    for character in code.chars() {
        lexer.lex(character);
    }

    lexer.finish();
    println!("{:?}", lexer);
}
