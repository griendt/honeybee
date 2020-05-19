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
    pub(crate) data_type: TokenType,
    pub(crate) value: String,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.data_type, self.value)
    }
}