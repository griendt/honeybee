use core::panicking::panic_fmt;

use crate::token::{Token, TokenType};

const ENCAPSULATORS_LEFT: [char; 4] = [
    '{',
    '(',
    '[',
    '<',
];

const ENCAPSULATORS_RIGHT: [char; 4] = [
    '}',
    ')',
    ']',
    '>',
];

const OPERATORS: [char; 1] = [
    '=',
];

const SEPARATORS: [char; 2] = [
    ',',
    ';',
];

#[derive(Debug, PartialEq)]
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

    pub fn lex(&mut self, character: char) -> () {
        let new_state = self.get_next_state(character);

        if new_state == self.state || self.state == State::None
        {
            // We remain in the same state, or we enter any state from the None state.
            // This means we can append the character to
            // our register and move on to the next character.
            self.register.push(character);
        }
        else {
            // We are switching states. In every case, that means we have to flush the
            // current register according to the current state. Then we can switch
            // safely and start the new token with a fresh register.
            self.end_token();

            // We must not forget about the current character! Take it as the first
            // character of the new token. Note that we can (and should) omit this if
            // the new state is a None state, because that means we are dealing with
            // whitespace which we should strip from the upcoming token.
            if new_state != State::None {
                self.register.push(character);
            }
        }
        self.state = new_state;
    }

    pub fn finish(&mut self) {
        // We will stop lexing. Finish whatever token that was
        // still in the register and clear the inner state.
        // Note that we will retain the list of tokens for use.
        self.end_token();
        self.state = State::None;
    }

    fn end_token(&mut self) {
        let token_type = match self.state {
            State::Identifier => TokenType::Identifier,
            State::None => TokenType::None,

            // For each state, we must determine what type of token
            // we should add as the end the current token.
            _ => unimplemented!(),
        };

        if token_type != TokenType::None {
            // Any token that has a type other than None, will be appended to the list.
            // If we wish to add a null token, use the Null token type instead.
            let token = Token { data_type: token_type, value: self.register.clone() };
            self.tokens.push(token);
        }

        // Clear the register so we can start a new token.
        self.register = "".to_string();
    }

    fn get_next_state(&self, character: char) -> State {
        match self.state {
            State::None | State::Identifier => {
                if
                    ENCAPSULATORS_LEFT.contains(&character) ||
                    ENCAPSULATORS_RIGHT.contains(&character) ||
                    OPERATORS.contains(&character)
                {
                    // We are using the operator state here for encapsulators, despite the
                    // intuition that we should move to a special "scope" state. This is
                    // because encapsulators might also act as actual operators in a certain sense.
                    return State::Operator;
                }

                if SEPARATORS.contains(&character) {
                    return State::Separator;
                }

                if character.is_alphabetic() {
                    // If we are dealing with an alphabetic character, it must be the name
                    // of some variable, identifier, or keyword.
                    return State::Identifier;
                }

                match character {
                    ' ' => State::None,
                    '\'' => State::Character,
                    '"' => State::String,
                    ':' => State::Colon,
                    _ => panic!(format!("Could not find a suitable state for character: '{}'", character))
                }
            }
            _ => unimplemented!()
        }
    }
}