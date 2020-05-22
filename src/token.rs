use std::fmt;
use std::fmt::Formatter;
use crate::ast::HoneyValue;

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

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
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


#[derive(Debug, Clone)]
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

    pub fn to_honey_value(&self) -> HoneyValue {
        match self._type {
            Some(TokenType::NumericLiteral) => HoneyValue::Number(self.value.parse::<u64>().unwrap()),
            _ => panic!("Could not convert token with type {:#?} to HoneyValue", self._type),
        }
    }

    pub fn setValue(&self, value: String) -> Token {
        let mut new_token = self.clone();
        new_token.value = value;
        new_token
    }

    pub fn setCategory(&self, category: TokenCategory) -> Token {
        let mut new_token = self.clone();
        new_token.category = category;
        new_token
    }

    pub fn setType(&self, _type: TokenType) -> Token {
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