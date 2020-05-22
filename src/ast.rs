use std::borrow::Borrow;
use std::collections::HashMap;

use crate::token::{Token, TokenCategory, TokenType};

#[derive(Debug)]
pub struct AST {
    pub scope: HashMap<String, HoneyValue>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum HoneyValue {
    Number(u64),
}

#[derive(Debug, Clone, PartialEq)]
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

    pub fn from_tokens(tokens: Vec<Token>) -> Self {
        let mut statement = Statement { tokens: vec![], _type: None };
        statement.tokens = tokens;
        statement._type = Some(statement.determine_type());
        statement
    }

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

    pub fn execute(&self, scope: &mut HashMap<String, HoneyValue>) -> Option<HoneyValue> {
        // We should probably require a context or scope for the statement as an argument.
        // Right now we won't care about that and just execute whatever is inside the statement
        // as if it were the only part of the entire program.
        match self._type {
            Some(StatementType::CodeBlock) => {
                // If we're dealing with a code block, split it
                // into sub-statements, and execute each of them.
                // Note that this is a recursive call!
                let statements = self.get_statements_from_code_block();
                let mut return_value = None;
                for statement in statements {
                    return_value = statement.execute(scope);
                }

                return_value
            },
            Some(StatementType::Assignment) => self.execute_assignment(scope),
            Some(StatementType::Leaf) => Some(self.tokens[0].to_honey_value(scope)),

            // We could, in theory, determine the type on the go, but this should never
            // be needed. Therefore we will explicitly panic because it implies bad
            // implementation at some point during the parsing of the tokens.
            None => panic!("Cannot execute statement whose type was not evaluated"),

            _ => panic!("Unsupported statement type: {:?}", self._type)
        }
    }

    fn execute_assignment(&self, scope: &mut HashMap<String, HoneyValue>) -> Option<HoneyValue> {
        // An assignment consists of two parts: the left side, which should be an identifier,
        // and the right side, which should be an expression that reduces to a single value.
        // For now we only support elementary identifiers on the left side (i.e. one token).
        // We should extend this, of course, to implement assignments such as foo.bar = baz.
        // However, this extension COULD be implemented in a part of code that gets executed
        // before this function is reached.
        if self.tokens[0]._type != Some(TokenType::VariableName) {
            panic!("Cannot assign to non-variable value: {}", self.tokens[0].value)
        }

        assert_eq!(self.tokens[0]._type, Some(TokenType::VariableName));
        assert_eq!(self.tokens[1]._type, Some(TokenType::AssignmentOperator));
        assert!([TokenType::VariableName, TokenType::NumericLiteral]
            .contains(&self.tokens[2]._type.unwrap()));

        // This is probably not the best check to ask whether we are dealing with an elementary
        // expression... the left- and right hand sides should probably already be split before
        // we even reach this part of the code, and we should ask of each side whether they are
        // elementary (and if not, execute the sub-expressions first). But right now we are only
        // supporting elementary expressions.

        let value = match self.tokens.len() {
            // An elementary assignment: the right-hand side is a single token
            4 => self.tokens[2].to_honey_value(scope),
            _ => { Statement::from_tokens(self.tokens[2..].to_owned())
                .execute(scope)
                .unwrap()
            }
        };

        scope.insert(self.tokens[0].value.clone(), value);
        Some(value)
    }

    // A CodeBlock-type statement contains sub-statements.
    // This method splits that statement into a vector of its sub-statements.
    fn get_statements_from_code_block(&self) -> Vec<Statement> {
        assert_eq!(self._type, Some(StatementType::CodeBlock));

        let mut statements: Vec<Statement> = vec![];
        let statement = &mut Statement { tokens: vec![], _type: None };
        for token in self.tokens.iter() {
            statement.tokens.push(token.clone());
            if token._type == Some(TokenType::StatementSeparator) {
                statement._type = Some(statement.determine_type());
                statements.push((&*statement).clone());
                statement.tokens = vec!();
            }
        }

        statements
    }
}

#[derive(Debug)]
pub struct CodeBlock {
    pub statements: Vec<Statement>,
}

impl AST {
    pub fn new() -> AST {
        let scope: HashMap<String, HoneyValue> = HashMap::new();
        AST { scope }
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

    pub fn make(&mut self, tokens: &mut Vec<Token>) -> () {
        // First, parse the tokens into their more concrete types.
        tokens.iter_mut().for_each(|token| self.parse_token(token));

        // Turn immutable
        let tokens = &*tokens;

        // Any program is really a code block. So, we will create a CodeBlock
        // type statement as the root node of the abstract syntax tree.
        // When executing the code, the tree will expand into sub-statements.
        let code_block = &mut Statement {
            tokens: tokens.clone(),
            _type: Some(StatementType::CodeBlock)
        };

        code_block.execute(&mut self.scope);
    }
}

#[cfg(test)]
mod test {
    use crate::ast::HoneyValue::Number;

    use super::*;

    #[test]
    fn it_recognizes_token_types_in_a_simple_assignment() {
        // Corresponds to the following source code:
        // x=3;
        let tokens: &mut Vec<Token> = &mut vec!(
            Token::new()
                .set_category(TokenCategory::Identifier)
                .set_value(String::from("x")),
            Token::new()
                .set_category(TokenCategory::Operator)
                .set_value(String::from("=")),
            Token::new()
                .set_category(TokenCategory::Literal)
                .set_value(String::from("3")),
            Token::new()
                .set_category(TokenCategory::Separator)
                .set_value(String::from(";")),
        );

        let ast = AST::new();
        tokens.iter_mut().for_each(|token| ast.parse_token(token));

        assert_eq!(tokens[0]._type, Some(TokenType::VariableName));
        assert_eq!(tokens[1]._type, Some(TokenType::AssignmentOperator));
        assert_eq!(tokens[2]._type, Some(TokenType::NumericLiteral));
        assert_eq!(tokens[3]._type, Some(TokenType::StatementSeparator));
    }

    #[test]
    fn it_parses_a_simple_assignment() {
        // Corresponds to the following source code:
        // x=3;
        let tokens: &mut Vec<Token> = &mut vec!(
            Token::new()
                .set_category(TokenCategory::Identifier)
                .set_value(String::from("x")),
            Token::new()
                .set_category(TokenCategory::Operator)
                .set_value(String::from("=")),
            Token::new()
                .set_category(TokenCategory::Literal)
                .set_value(String::from("3")),
            Token::new()
                .set_category(TokenCategory::Separator)
                .set_value(String::from(";")),
        );

        let mut ast = AST::new();
        ast.make(tokens);

        assert_eq!(ast.scope.get("x"), Some(&Number(3)));
    }

    #[test]
    fn accessing_uninitialized_variable_yields_a_none() {
        let ast = AST::new();
        assert_eq!(ast.scope.get("x"), None);
    }

    #[test]
    fn it_allows_variable_redefinition() {
        // Corresponds to the following source code:
        // x=3;x=4;
        let tokens: &mut Vec<Token> = &mut vec!(
            Token::new()
                .set_category(TokenCategory::Identifier)
                .set_value(String::from("x")),
            Token::new()
                .set_category(TokenCategory::Operator)
                .set_value(String::from("=")),
            Token::new()
                .set_category(TokenCategory::Literal)
                .set_value(String::from("3")),
            Token::new()
                .set_category(TokenCategory::Separator)
                .set_value(String::from(";")),
            Token::new()
                .set_category(TokenCategory::Identifier)
                .set_value(String::from("x")),
            Token::new()
                .set_category(TokenCategory::Operator)
                .set_value(String::from("=")),
            Token::new()
                .set_category(TokenCategory::Literal)
                .set_value(String::from("4")),
            Token::new()
                .set_category(TokenCategory::Separator)
                .set_value(String::from(";")),
        );

        let mut ast = AST::new();
        ast.make(tokens);

        assert_eq!(ast.scope.get("x"), Some(&Number(4)));
    }

    #[test]
    fn it_can_assign_a_variable_to_another_variable() {
        // Corresponds to the following source code:
        // x=3;x=4;x=y;
        let tokens: &mut Vec<Token> = &mut vec!(
            Token::new()
                .set_category(TokenCategory::Identifier)
                .set_value(String::from("x")),
            Token::new()
                .set_category(TokenCategory::Operator)
                .set_value(String::from("=")),
            Token::new()
                .set_category(TokenCategory::Literal)
                .set_value(String::from("3")),
            Token::new()
                .set_category(TokenCategory::Separator)
                .set_value(String::from(";")),
            Token::new()
                .set_category(TokenCategory::Identifier)
                .set_value(String::from("y")),
            Token::new()
                .set_category(TokenCategory::Operator)
                .set_value(String::from("=")),
            Token::new()
                .set_category(TokenCategory::Literal)
                .set_value(String::from("4")),
            Token::new()
                .set_category(TokenCategory::Separator)
                .set_value(String::from(";")),
            Token::new()
                .set_category(TokenCategory::Identifier)
                .set_value(String::from("x")),
            Token::new()
                .set_category(TokenCategory::Operator)
                .set_value(String::from("=")),
            Token::new()
                .set_category(TokenCategory::Identifier)
                .set_value(String::from("y")),
            Token::new()
                .set_category(TokenCategory::Separator)
                .set_value(String::from(";")),
        );

        let mut ast = AST::new();
        ast.make(tokens);

        assert_eq!(ast.scope.get("x"), Some(&Number(4)));
        assert_eq!(ast.scope.get("y"), Some(&Number(4)));
    }

    #[test]
    fn it_can_assign_a_new_value_after_being_assigned_to_another_variable() {
        // Corresponds to the following source code:
        // x=3;x=4;x=y;x=5;
        let tokens: &mut Vec<Token> = &mut vec!(
            Token::new()
                .set_category(TokenCategory::Identifier)
                .set_value(String::from("x")),
            Token::new()
                .set_category(TokenCategory::Operator)
                .set_value(String::from("=")),
            Token::new()
                .set_category(TokenCategory::Literal)
                .set_value(String::from("3")),
            Token::new()
                .set_category(TokenCategory::Separator)
                .set_value(String::from(";")),
            Token::new()
                .set_category(TokenCategory::Identifier)
                .set_value(String::from("y")),
            Token::new()
                .set_category(TokenCategory::Operator)
                .set_value(String::from("=")),
            Token::new()
                .set_category(TokenCategory::Literal)
                .set_value(String::from("4")),
            Token::new()
                .set_category(TokenCategory::Separator)
                .set_value(String::from(";")),
            Token::new()
                .set_category(TokenCategory::Identifier)
                .set_value(String::from("x")),
            Token::new()
                .set_category(TokenCategory::Operator)
                .set_value(String::from("=")),
            Token::new()
                .set_category(TokenCategory::Identifier)
                .set_value(String::from("y")),
            Token::new()
                .set_category(TokenCategory::Separator)
                .set_value(String::from(";")),
            Token::new()
                .set_category(TokenCategory::Identifier)
                .set_value(String::from("x")),
            Token::new()
                .set_category(TokenCategory::Operator)
                .set_value(String::from("=")),
            Token::new()
                .set_category(TokenCategory::Literal)
                .set_value(String::from("5")),
            Token::new()
                .set_category(TokenCategory::Separator)
                .set_value(String::from(";")),
        );

        let mut ast = AST::new();
        ast.make(tokens);

        assert_eq!(ast.scope.get("x"), Some(&Number(5)));
        assert_eq!(ast.scope.get("y"), Some(&Number(4)));
    }
}