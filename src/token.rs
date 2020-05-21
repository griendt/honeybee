use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, PartialEq)]
pub enum TokenType {
    None,         // Dummy token
    Null,         // NULL
    Identifier,   // Name of a variable or function
    Operator,     // Tokens that represent operators
    Separator,    // A separation token
    String,       // A string literal
    Numeric,      // An integer, float or decimal literal
    Atom,         // An atom
    Error,        // Something bad happened (parse error)
}
impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug)]
pub struct Token {
    pub(crate) token_type: TokenType,
    pub(crate) value: String,

    // Metadata
    pub(crate) file: Option<String>,
    pub(crate) line: u32,
    pub(crate) column: u32,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.token_type, self.value)
    }
}