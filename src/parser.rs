use crate::lexer::Lexer;

const ENCAPSULATORS: [&str; 4]  = [
    "{}",
    "()",
    "[]",
    "<>" ,
];

const OPERATORS: [&str; 1] = [
    "=",
];

const SEPARATORS: [&str; 2] = [
    ",",
    ";",
];

pub fn parse(code: String) {
    println!("The code contains:\n  {}", code);

    let mut lexer = Lexer::new();
    for character in code.chars() {
        lexer.lex(character);

    }
    println!("{:?}", lexer)
}
