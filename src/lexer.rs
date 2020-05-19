use crate::token::Token;

#[derive(Debug)]
enum State {
    // Start of file
    None,
    // Name of a variable or a function
    Identifier,
    // Symbols that represent operators
    Operator,
    // Symbols that represent separators
    Separator,
    // Reading between single quotes
    Character,
    // Reading between double quotes
    String,
    // In escaped mode while reading between single quotes
    CharacterEscape,
    // In escaped mode while reading between double quotes
    StringEscape,
    // A numeric value
    Numeric,
    // A number after a decimal dot
    NumericDecimal,
    // An atom or some operator using the colon symbol
    Colon,
    // An atom
    Atom,
}

#[derive(Debug)]
pub struct Lexer {
    // The current state of the lexer
    state: State,
    // A sequence of characters that form an incomplete token
    register: String,
    pub tokens: Vec<Token>,
}

impl Lexer {

    // Instantiate a new lexer.
    pub fn new() -> Lexer {
        Lexer { state: State::None, register: "".to_string(), tokens: vec![] }
    }

    pub fn lex(&mut self, character: char) {
        match self.state {
            State::None => self.begin(character),
            _ => unimplemented!(),
        }
    }

    fn begin(&mut self, character: char) -> () {
        self.state = State::String; // TODO: Change this accoring to the kind of character
        self.register = character.to_string();
    }
}