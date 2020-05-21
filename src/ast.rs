use crate::token::{Token, TokenCategory, TokenType};
use std::borrow::Borrow;

#[derive(Debug)]
pub struct AST {}

impl AST {
    pub fn new() -> AST {
        AST {}
    }

    pub fn parse_token(&self, token: &mut Token) -> () {
        // Depending on the context, a token may mean different things.
        // We will assume that this context is stored in self.

        token._type = match (token.category.borrow(), token.value.borrow()) {
            // If we are dealing with a semicolon separator, it's a statement separator.
            (TokenCategory::Separator, ";") => Option::from(TokenType::StatementSeparator),

            // Right now we assume all identifiers are variables. Obviously this is not
            // actually true because all functions, built-in or otherwise, and keywords
            // are currently also lexed as identifiers.
            (TokenCategory::Identifier, _) => Option::from(TokenType::VariableName),

            (TokenCategory::Operator, "=") => Option::from(TokenType::AssignmentOperator),
            (TokenCategory::Operator, "+") => Option::from(TokenType::SumOperator),

            // Right now the only literal we will support is the numeric one.
            // Obviously we also need to support other literals such as booleans, strings,
            // null, and so forth. We should check the value for this rather than passing
            // the underscore _ as a wildcard!
            (TokenCategory::Literal, _) => Option::from(TokenType::NumericLiteral),
            _ => panic!("Unsupported combination of token category and value: {} {}", token.category, token.value),
        };
    }

    pub fn make(&self, tokens: &mut Vec<Token>) {
        // Any program is really a code block. Split up all tokens into a list
        // of statements which should be parsed and executed sequentially.
        // let &mut codeBlock = CodeBlock { statements: vec![] };

        // let &mut statement = Statement { };

        for token in tokens {
            // For now we will not support nesting. Just do a naive split based on the
            // semi-colon separator.

            self.parse_token(token);
        }

    }
}