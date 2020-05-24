use std::fmt;
use std::fmt::Formatter;
use crate::ast::HoneyValue;
use std::collections::HashMap;
use std::borrow::Borrow;
use std::process::exit;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenCategory {
    None,         // Dummy token
    // Null,         // NULL
    Identifier,   // Name of a variable or function
    Operator,     // Tokens that represent operators
    Separator,    // A separation token
    Literal,      // An integer, float or decimal literal or a string
    // Atom,         // An atom
    Error,        // Something bad happened (parse error)
}
impl fmt::Display for TokenCategory {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType {
    Keyword,
    VariableName,
    AssignmentOperator,
    SumOperator,
    NumericLiteral,
    StatementSeparator,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    // The value of the token as it is present in the source code.
    pub(crate) value: String,

    // A basic category for the token, assigned before parsing.
    pub(crate) category: TokenCategory,

    // A concrete type of token, assigned only during parsing.
    pub(crate) _type: Option<TokenType>,

    // Metadata
    pub(crate) file: Option<String>,
    pub(crate) line: u32,
    pub(crate) column: u32,
}

impl Token {

    // This is a convenience method during unit testing.
    // Hence we will annotate this method to avoid compiler warnings.
    #[allow(dead_code)]
    pub fn new() -> Token {
        Token {
            value: "".to_string(),
            category: TokenCategory::None,
            _type: None,
            file: None,
            line: 0,
            column: 0
        }
    }

    pub fn make_error(&self, message: String) -> String {
        let full_message = format!("{} at line {} column {}{}",
            message,
            self.line,
            self.column,
            match self.file.borrow() {
                Some(file) => format!(" in {}", file),
                None => String::from(""),
            }
        );

        full_message
    }

    pub fn to_honey_value(&self, scope: &HashMap<String, HoneyValue>) -> HoneyValue {
        match self._type {
            Some(TokenType::NumericLiteral) => {
                match self.value.parse::<u64>() {
                    Ok(x) => HoneyValue::Number(x),
                    Err(_) => panic!("Could not cast '{}' to u64", self.value),
                }
            },
            Some(TokenType::VariableName) => match scope.get(self.value.as_str()) {
                Some(x) => x.clone(),
                None => {
                    self.make_error(format!("Undefined variable: {}", self.value));
                    exit(1);
                },
            },
            _ => panic!("Could not convert token with type {:#?} to HoneyValue", self._type.unwrap()),
        }
    }

    // This is a convenience method during unit testing.
    // Hence we will annotate this method to avoid compiler warnings.
    #[allow(dead_code)]
    pub fn set_value(&self, value: String) -> Token {
        let mut new_token = self.clone();
        new_token.value = value;
        new_token
    }

    // This is a convenience method during unit testing.
    // Hence we will annotate this method to avoid compiler warnings.
    #[allow(dead_code)]
    pub fn set_category(&self, category: TokenCategory) -> Token {
        let mut new_token = self.clone();
        new_token.category = category;
        new_token
    }

    // This is a convenience method during unit testing.
    // Hence we will annotate this method to avoid compiler warnings.
    #[allow(dead_code)]
    pub fn set_type(&self, _type: TokenType) -> Token {
        let mut new_token = self.clone();
        new_token._type = Some(_type);
        new_token
    }
}
impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.category, self.value)
    }
}