use std::borrow::Borrow;

use crate::io::{error, warn};
use crate::token::{Token, TokenCategory};

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

const OPERATORS: [char; 5] = [
    '=',
    '!',
    '-',
    '+',
    '/',
];

const SEPARATORS: [char; 2] = [
    ',',
    ';',
];

#[derive(Debug, PartialEq)]
enum State {
    // Start of file
    None,
    // Parse error
    Error,
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
    // The decimal dot
    DecimalSeparator,
    // A number after a decimal dot
    NumericDecimal,
    // An atom or some operator using the colon symbol
    Colon,
    // An atom
    Atom,
}

#[derive(Debug)]
pub struct Lexer {
    // The sequence of lexed tokens
    pub tokens: Vec<Token>,
    // The current state of the lexer
    state: State,
    // A sequence of characters that form an incomplete token
    register: String,

    // Metadata for error messages and debugging
    pub file: Option<String>,
    line: u32,
    column: u32,

    verbose: bool,
}

impl Lexer {
    // Instantiate a new lexer.
    pub fn new() -> Lexer {
        Lexer {
            state: State::None,
            register: String::from(""),
            tokens: vec![],
            file: None,
            line: 1,
            column: 1,
            verbose: true,
        }
    }

    pub fn lex(&mut self, character: char) -> () {
        let new_state = self.get_next_state(character);

        if (new_state == self.state && self.state != State::None)
            || (self.state == State::None && new_state != State::None)
            || (self.state == State::Numeric && new_state == State::DecimalSeparator)
            || (self.state == State::DecimalSeparator && new_state == State::NumericDecimal)
        {
            // We remain in the same state, or we enter any state from the None state,
            // or we are parsing a decimal number.
            // This means we can append the character to
            // our register and move on to the next character.
            self.register.push(character);
        } else {
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

        self.column += 1;
        // TODO: Detect whether we have found a newline. If so,
        // reset the column counter and increment the line counter.

        self.state = new_state;
    }

    pub fn finish(&mut self) {
        // We will stop lexing. Finish whatever token that was
        // still in the register and clear the inner state.
        // Note that we will retain the list of tokens for use.
        self.end_token();

        if self.state != State::Error {
            // We will keep the error state so that the caller can check
            // whether we encountered any lexing errors during parsing.
            // If lexing was successful, the state will be cleared properly.
            self.state = State::None;
        }
    }

    pub fn pretty_print_tokens(&self) {
        for token in self.tokens.iter() {
            println!(
                "{:>3}.{:<3} {:>10}: {:<6} {}",
                token.line,
                token.column,
                format!("{}", token.category),
                token.value,
                match token._type.borrow() {
                    Some(x) => format!("({})", x),
                    None => String::from(""),
                });
        }
    }

    fn print_error(&self, character: char) -> () {
        if !self.verbose { return; }
        error(format!("Syntax error at line {}, column {}{}: '{}'",
                      self.line,
                      self.column,
                      match self.file.clone() {
                          Some(file) => format!(" in {}", file),
                          None => String::from(""),
                      },
                      character
        ).as_str());
    }

    fn end_token(&mut self) {
        let token_category = match self.state {
            State::Separator => TokenCategory::Separator,
            State::Operator => TokenCategory::Operator,
            State::Numeric | State::NumericDecimal | State::DecimalSeparator => TokenCategory::Literal,
            State::Identifier => TokenCategory::Identifier,
            State::None => TokenCategory::None,
            State::Error => TokenCategory::Error,

            // For each state, we must determine what type of token
            // we should add as the end the current token.
            _ => {
                error(format!("Unsupported state: {:?}", self.state).as_str());
                TokenCategory::Error
            }
        };

        if ![TokenCategory::None, TokenCategory::Error].contains(&token_category) {
            // Any token that has a type other than None or Error, will be appended to the list.
            // If we wish to add a null token, use the Null token type instead.
            let token = Token {
                category: token_category,
                _type: None,
                value: self.register.clone(),
                file: self.file.clone(),
                line: self.line,

                // TODO: This works unless one token consists of multiple lines.
                column: self.column - self.register.len() as u32,
            };
            self.tokens.push(token);
        }

        // Clear the register so we can start a new token.
        self.register = "".to_string();
    }

    fn get_default_state(&self, character: char) -> State {
        if ENCAPSULATORS_LEFT.contains(&character) ||
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

        if character.is_numeric() {
            return State::Numeric;
        }

        if character.is_alphabetic() {
            // If we are dealing with an alphabetic character, it must be the name
            // of some variable, identifier, or keyword.
            return State::Identifier;
        }

        match character {
            ' ' | '\n' | '\r' => State::None,
            '\'' => State::Character,
            '"' => State::String,
            ':' => State::Colon,
            _ => {
                self.print_error(character);
                State::Error
            }
        }
    }

    fn get_next_state(&self, character: char) -> State {
        match self.state {
            State::None | State::Identifier | State::Separator => self.get_default_state(character),
            State::Operator => {
                // If we are in an operator state, we will accept more characters that look
                // operator-like to form multi-character operators. For example +=, **, ->.
                // But we also want to accept certain other characters while in thie state, such
                // as > so that we can implement arrows like -> and =>. We should add those
                // conditionals here rather than just going to the default state.
                self.get_default_state(character)
            }
            State::Numeric => {
                if character == '.' {
                    // We are dealing with a decimal dot. We must make sure to parse this dot
                    // as a decimal dot and not as something else such as an operator.
                    return State::DecimalSeparator;
                }

                self.get_default_state(character)
            }
            State::DecimalSeparator | State::NumericDecimal => {
                if character.is_numeric() {
                    // We are dealing with the decimal part of a number.
                    // Note that we do not go back to the Numeric state because it would allow
                    // for illegal separator chaining such as 1.34.590 which is not a number.
                    return State::NumericDecimal;
                }

                self.get_default_state(character)
            }
            State::Error => State::Error, // Propagate errors, do not try to recover
            _ => unimplemented!("{:?}", self.state)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn parse(lexer: &mut Lexer, code: &str) {
        for character in code.chars() {
            lexer.lex(character);
        }
        lexer.finish();
    }

    #[test]
    fn it_lexes_a_string() {
        let mut lexer = Lexer::new();
        parse(&mut lexer, "foo");

        assert_eq!(lexer.state, State::None);
        assert_eq!(lexer.tokens.len(), 1);
        assert_eq!(lexer.tokens[0].value, "foo");
        assert_eq!(lexer.tokens[0].category, TokenCategory::Identifier);
    }

    #[test]
    fn it_lexes_an_integer() {
        let mut lexer = Lexer::new();
        parse(&mut lexer, "3");

        assert_eq!(lexer.state, State::None);
        assert_eq!(lexer.tokens.len(), 1);
        assert_eq!(lexer.tokens[0].category, TokenCategory::Literal);
        assert_eq!(lexer.tokens[0].value, "3");
    }

    #[test]
    fn it_lexes_a_float_without_decimal_part() {
        let mut lexer = Lexer::new();
        parse(&mut lexer, "2.");

        assert_eq!(lexer.state, State::None);
        assert_eq!(lexer.tokens.len(), 1);
        assert_eq!(lexer.tokens[0].category, TokenCategory::Literal);
        assert_eq!(lexer.tokens[0].value, "2.");
    }

    #[test]
    fn it_lexes_a_float_with_decimal_part() {
        let mut lexer = Lexer::new();
        parse(&mut lexer, "2.34");

        assert_eq!(lexer.state, State::None);
        assert_eq!(lexer.tokens.len(), 1);
        assert_eq!(lexer.tokens[0].category, TokenCategory::Literal);
        assert_eq!(lexer.tokens[0].value, "2.34");
    }

    #[test]
    fn it_errors_at_malformed_number() {
        let mut lexer = Lexer::new();
        lexer.verbose = false; // Do not print the syntax error message to stdout during testing
        parse(&mut lexer, "2.345.6");

        assert_eq!(lexer.state, State::Error);
    }

    #[test]
    fn it_lexes_a_simple_arithmetic_expression() {
        let mut lexer = Lexer::new();
        parse(&mut lexer, "1 + 2 = 3");
        assert_eq!(lexer.state, State::None);
        assert_eq!(lexer.tokens.len(), 5);
        assert_eq!(lexer.tokens[0].category, TokenCategory::Literal);
        assert_eq!(lexer.tokens[0].value, "1");
        assert_eq!(lexer.tokens[1].category, TokenCategory::Operator);
        assert_eq!(lexer.tokens[1].value, "+");
        assert_eq!(lexer.tokens[2].category, TokenCategory::Literal);
        assert_eq!(lexer.tokens[2].value, "2");
        assert_eq!(lexer.tokens[3].category, TokenCategory::Operator);
        assert_eq!(lexer.tokens[3].value, "=");
        assert_eq!(lexer.tokens[4].category, TokenCategory::Literal);
        assert_eq!(lexer.tokens[4].value, "3");
    }

    #[test]
    fn it_lexes_statements_without_whitespace() {
        let mut lexer = Lexer::new();
        parse(&mut lexer, "22.+9.8");
        assert_eq!(lexer.state, State::None);
        assert_eq!(lexer.tokens.len(), 3);
        assert_eq!(lexer.tokens[0].category, TokenCategory::Literal);
        assert_eq!(lexer.tokens[0].value, "22.");
        assert_eq!(lexer.tokens[1].category, TokenCategory::Operator);
        assert_eq!(lexer.tokens[1].value, "+");
        assert_eq!(lexer.tokens[2].category, TokenCategory::Literal);
        assert_eq!(lexer.tokens[2].value, "9.8");
    }

    #[test]
    fn it_handles_newlines() {
        let mut lexer = Lexer::new();
        parse(&mut lexer, "2=3\n4");
        assert_eq!(lexer.tokens.len(), 4);
        assert_eq!(lexer.tokens[0].category, TokenCategory::Literal);
        assert_eq!(lexer.tokens[0].value, "2");
        assert_eq!(lexer.tokens[1].category, TokenCategory::Operator);
        assert_eq!(lexer.tokens[1].value, "=");
        assert_eq!(lexer.tokens[2].category, TokenCategory::Literal);
        assert_eq!(lexer.tokens[2].value, "3");
        assert_eq!(lexer.tokens[3].category, TokenCategory::Literal);
        assert_eq!(lexer.tokens[3].value, "4");
    }
}