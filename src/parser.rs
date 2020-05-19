use crate::lexer::Lexer;


pub fn parse(code: String) {
    println!("The code contains:\n  {}", code);

    let mut lexer = Lexer::new();
    for character in code.chars() {
        lexer.lex(character);
    }

    lexer.finish();
    println!("{:?}", lexer);
}
