use crate::token::{Token, TokenCategory, TokenType};
use std::borrow::Borrow;
use crate::io::info;

#[derive(Debug)]
pub struct AST {}

#[derive(Debug, Clone)]
pub enum StatementType {
    Error,
    Leaf,
    Assignment,
    CodeBlock,
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub tokens: Vec<Token>,
    pub _type: Option<StatementType>,
}

impl Statement {
    pub fn determine_type(&self) -> StatementType {
        if self.tokens.len() == 1 {
            return StatementType::Leaf;
        }

        // This is not a great check: we turn a statement into an assignment if it contains
        // an assignment operator. This, of course, doesn't work well in case this operator
        // has a lower precedence than some other (for example because it's in parantheses).
        if !(self.tokens.clone().into_iter()
            .filter(|token| token._type == Some(TokenType::AssignmentOperator))
            .collect::<Vec<Token>>()
            .is_empty())
        {
            return StatementType::Assignment;
        }

        StatementType::Error
    }
}

#[derive(Debug)]
pub struct CodeBlock {
    pub statements: Vec<Statement>,
}

impl AST {
    pub fn new() -> AST {
        AST {}
    }

    pub fn parse_token(&self, token: &mut Token) -> () {
        // Depending on the context, a token may mean different things.
        // We will assume that this context is stored in self.
        // Currently we are not using any context, but we definitely will in the future!

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
        // First, parse the tokens into their more concrete types.
        tokens.iter_mut().for_each(|token| self.parse_token(token));

        // Turn immutable
        let tokens = &*tokens;

        // Any program is really a code block. Split up all tokens into a list
        // of statements which should be parsed and executed sequentially.
        // let &mut codeBlock = CodeBlock { statements: vec![] };
        let code_block = &mut CodeBlock { statements: vec!() };

        let statement = &mut Statement { tokens: vec![], _type: None };

        for token in tokens {
            statement.tokens.push(token.clone());
            if token._type == Some(TokenType::StatementSeparator) {
                statement._type = Some(statement.determine_type());
                code_block.statements.push((&*statement).clone());
                statement.tokens = vec!();
            }
        }

        info("Code block:");
        println!("{:#?}", code_block);
    }

    pub fn interpret(&self, code_block: CodeBlock) -> () {
        for statement in code_block.statements {

        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_recognizes_token_types_in_a_simple_assignment() {
        let tokens: &mut Vec<Token> = &mut vec!(
            Token::new()
                .setCategory(TokenCategory::Identifier)
                .setValue(String::from("x")),
            Token::new()
                .setCategory(TokenCategory::Operator)
                .setValue(String::from("=")),
            Token::new()
                .setCategory(TokenCategory::Literal)
                .setValue(String::from("3")),
            Token::new()
                .setCategory(TokenCategory::Separator)
                .setValue(String::from(";")),
        );

        let ast = AST::new();
        tokens.iter_mut().for_each(|token| ast.parse_token(token));

        assert_eq!(tokens[0]._type, Some(TokenType::VariableName));
        assert_eq!(tokens[1]._type, Some(TokenType::AssignmentOperator));
        assert_eq!(tokens[2]._type, Some(TokenType::NumericLiteral));
        assert_eq!(tokens[3]._type, Some(TokenType::StatementSeparator));

    }
}