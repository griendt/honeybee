use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, PartialEq)]
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

#[derive(Debug)]
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


#[derive(Debug)]
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

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.category, self.value)
    }
}