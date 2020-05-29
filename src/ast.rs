use std::borrow::Borrow;
use std::collections::HashMap;
use std::option::Option::None;
use std::process::exit;

use crate::ast::StatementContent::Declaration;
use crate::io::{error, info};
use crate::token::{Token, TokenCategory, TokenType};
use crate::token::TokenType::DeclarationOperator;

const KEYWORDS: [&str; 0] = [];

#[derive(Debug)]
pub struct AST {
    pub scope: HashMap<String, HoneyValue>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum HoneyValue {
    Number(u64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementContent {
    // Uninitialized
    None,

    // We should expand on this type to allow multi-token variable assignment,
    // such as foo.bar. Now the left side is just a String.
    Leaf(String),

    // A declaration consists of a variable name (String) and a statement (actually an expression,
    // but we don't have those yet) that evaluates to a value upon execution.
    Declaration(String, Box<Statement>),

    // A CodeBlock a a vector of statements. Perhaps we will need to box these.
    CodeBlock(Vec<Statement>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    // A statement really should be just its content, but for debug purposes we
    // make it a struct that also has the tokens.
    pub tokens: Vec<Token>,
    pub content: StatementContent,
}

impl Statement {
    pub fn from_tokens(tokens: Vec<Token>) -> Result<Statement, String> {
        let mut statement = Statement { tokens: vec![], content: StatementContent::None };
        statement.tokens = tokens;
        match statement.set_content() {
            Ok(_) => Ok(statement),
            Err(e) => Err(e),
        }
    }

    pub fn set_content(&mut self) -> Result<(), String> {
        // This is not a great check: we turn a statement into an assignment if it contains
        // an assignment operator. This, of course, doesn't work well in case this operator
        // has a lower precedence than some other (for example because it's in parentheses).
        let declaration_position = self.tokens
            .iter()
            .position(|token| token._type == Some(DeclarationOperator));
        if declaration_position.is_some() {
            let pos = declaration_position.unwrap();
            // This is actually a bad assertion: only simple expressions pass this (e.g.
            // declarations where the left hand side is a single token).
            // But for now this is the only kind of declaration we will support.
            if pos == 1 {
                // Because we currently only support declaration to simple variables (1 token),
                // this must imply we are dealing with a " x := value" type statement.
                // We will not allow anything that is not a variable name.
                if self.tokens[0]._type != Some(TokenType::VariableName)
                {
                    let error = self.tokens[0].make_error(
                        format!(
                            "Syntax error: expected a variable name, found: '{}'",
                            String::from(self.tokens[0].value.clone())
                        )
                    );
                    return Err(error);
                }
            }
            else {
                // We can probably improve on this error. We are trying to parse an assignment,
                // but the equals sign isn't the second. We have little clue as to what
                // went wrong. Probably an assignment to a variable that isn't one token?
                let error = self.tokens[1].make_error(
                    String::from(format!(
                        "Syntax error: Unexpected token '{}', expected: '='",
                        self.tokens[2].value)
                    )
                );
                return Err(error);
            }

            // Make sure that the left hand side is, indeed, a variable and not something
            // else (such as a literal). Throw a syntax error otherwise.
            if self.tokens[pos - 1].category == TokenCategory::Literal {
                let error = self.tokens[pos - 1].make_error(
                    String::from("Syntax error: Cannot assign to literal")
                );
                return Err(error);
            }

            match Statement::from_tokens(
                self.tokens.clone()[pos + 1..]
                    .to_owned()
            ) {
                Ok(statement) => {
                    self.content = Declaration(
                        self.tokens[pos - 1].value.clone(),
                        Box::from(statement)
                    );
                    // self.tokens = vec![];   // We can now clear the tokens from memory.
                }
                Err(error) => return Err(error),
            };
        }

        Ok(())
    }

    pub fn parse(&mut self) {
        match self.content.borrow() {
            // Leaves need no further parsing
            StatementContent::Leaf(_) => {}
            StatementContent::CodeBlock(code_block_statement) => {
                let mut code_block_statement = code_block_statement.clone();
                for statement in code_block_statement.iter_mut() {
                    statement.parse();
                }

                self.content = StatementContent::CodeBlock(code_block_statement);
            }
            StatementContent::Declaration(name, statement) => {
                // Assignments have a value (=r.h.s.) which is composed of a statement on its own.
                // We should parse that statement as well.
                let mut assignment_statement = statement.clone();
                assignment_statement.parse();
                self.content = StatementContent::Declaration(name.parse().unwrap(), assignment_statement);
            }

            // Noop. TODO: Is this OK? It might allow for some strange syntax in sub-statements.
            StatementContent::None => {}
            _ => unimplemented!("Cannot parse this content type: '{:#?}'", self.content),
        }
    }

    pub fn execute(&self, scope: &mut HashMap<String, HoneyValue>)
                   -> Result<Option<HoneyValue>, String>
    {
        // We should probably require a context or scope for the statement as an argument.
        // Right now we won't care about that and just execute whatever is inside the statement
        // as if it were the only part of the entire program.
        match self.content.borrow() {
            StatementContent::CodeBlock(code_block_statement) => {
                let mut return_value: Result<Option<HoneyValue>, String> = Ok(None);

                let code_block_statement = code_block_statement.clone();

                // Execute each of the sub-statements.
                // Note that this is a recursive call!
                for statement in code_block_statement {
                    return_value = statement.execute(scope);

                    // If a statement leads to an error, abort the rest of
                    // the code block and return the error right away.
                    match return_value {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    };
                }

                return_value
            }
            StatementContent::Declaration(_, _) => self.execute_assignment(scope),

            // The var is not on the left side of an assignment, because if it were, it would have
            // been consumed in the execute_assignment function. Therefore we should be able
            // to expand the variable to its value.
            StatementContent::Leaf(name) =>
                match scope.get(name) {
                    Some(x) => Ok(Some(x.clone())),
                    None => {
                        self.tokens[0].make_error(format!("Undefined variable: '{}'", name));
                        exit(1);
                    }
                },

            // We could, in theory, determine the type on the go, but this should never
            // be needed. Therefore we will explicitly panic because it implies bad
            // implementation at some point during the parsing of the tokens.
            _ => panic!("Cannot execute statement whose content type was not evaluated"),
        }
    }

    fn execute_assignment(&self, scope: &mut HashMap<String, HoneyValue>)
                          -> Result<Option<HoneyValue>, String>
    {

        // This is probably not the best check to ask whether we are dealing with an elementary
        // expression... the left- and right hand sides should probably already be split before
        // we even reach this part of the code, and we should ask of each side whether they are
        // elementary (and if not, execute the sub-expressions first). But right now we are only
        // supporting elementary expressions.
        let value_result = match self.tokens.len() {
            // An elementary assignment: the right-hand side is a single token
            4 => Ok(Some(self.tokens[2].to_honey_value(scope))),
            _ => {
                let statement_result = Statement::from_tokens(self.tokens[3..].to_owned());
                match statement_result {
                    Ok(statement) => statement.execute(scope),
                    Err(error) => return Err(error),
                }
            }
        };

        match value_result {
            Ok(_) => {
                let value_to_insert = value_result.borrow().as_ref().unwrap().unwrap();
                scope.insert(self.tokens[0].value.clone(), value_to_insert);
                value_result
            }
            Err(_) => value_result,
        }
    }

    // A CodeBlock-type statement contains sub-statements.
    // This method splits that statement into a vector of its sub-statements,
    // and sets the content to these statements.
    fn set_statements_from_code_block(&mut self) -> Result<(), String> {
        match self.content {
            StatementContent::CodeBlock(_) => (),
            _ => panic!("Trying to parse a non-block statement as a code block"),
        };

        let mut statements: Vec<Statement> = vec![];
        let statement = &mut Statement { tokens: vec![], content: StatementContent::None };
        for token in self.tokens.iter() {
            statement.tokens.push(token.clone());
            if token._type == Some(TokenType::StatementSeparator) {
                match statement.set_content() {
                    Ok(_) => {
                        statements.push((&*statement).clone());
                        statement.tokens = vec!();
                    }
                    Err(error) => return Err(error),
                }
            }
        }

        self.content = StatementContent::CodeBlock(statements);
        Ok(())
    }
}


impl AST {
    pub fn new() -> AST {
        let scope: HashMap<String, HoneyValue> = HashMap::new();
        AST { scope }
    }

    pub fn parse_token(&self, token: &mut Token) -> Result<TokenType, String> {
        // Depending on the context, a token may mean different things.
        // We will assume that this context is stored in self.
        // Currently we are not using any context, but we definitely will in the future!

        let token_type = match (token.category.borrow(), token.value.borrow()) {
            // If we are dealing with a semicolon separator, it's a statement separator.
            (TokenCategory::Separator, ";") => TokenType::StatementSeparator,

            // Right now we assume all identifiers that aren't keywards
            // are variables. We will have to make sure all built-in keywords and functions
            // are matched. Obviously this is not very robust because user-defined
            // functions (for example) should not be parsed as variables either...
            (TokenCategory::Identifier, value) => match KEYWORDS.contains(&value) {
                true => TokenType::Keyword,
                false => TokenType::VariableName,
            },

            (TokenCategory::Operator, ":=") => TokenType::DeclarationOperator,
            (TokenCategory::Operator, "+") => TokenType::SumOperator,

            // Right now the only literal we will support is the numeric one.
            // Obviously we also need to support other literals such as booleans, strings,
            // null, and so forth. We should check the value for this rather than passing
            // the underscore _ as a wildcard!
            (TokenCategory::Literal, _) => TokenType::NumericLiteral,
            _ => return Err(format!("Unsupported combination of token category and value: {} {}", token.category, token.value)),
        };

        Ok(token_type)
    }

    pub fn build_and_run(&mut self, tokens: Vec<Token>) -> Result<Option<HoneyValue>, String> {
        // Any program is really a code block. So, we will create a CodeBlock
        // type statement as the root node of the abstract syntax tree.
        // Then we will parse the code block, which in turn will parse its sub-statements.
        let code_block = &mut Statement {
            tokens: tokens.clone(),
            content: StatementContent::CodeBlock(vec![]),
        };

        match code_block.set_statements_from_code_block() {
            Ok(_) => {}
            Err(error) => return Err(error)
        };

        code_block.parse();

        info(format!("Generated AST successfully").as_str());
        info(format!("{:#?}", code_block).as_str());

        let result = code_block.execute(&mut self.scope);
        match result.clone() {
            Ok(Some(value)) => info(format!(
                "Program executed successfully with return value: {:?}", value
            ).as_str()),
            Ok(None) => info("Program executed successfully with no return value"),
            Err(e) => error(format!("Program failed with error: {}", e).as_str())
        };

        result
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
        assert_eq!(tokens[1]._type, Some(TokenType::DeclarationOperator));
        assert_eq!(tokens[2]._type, Some(TokenType::NumericLiteral));
        assert_eq!(tokens[3]._type, Some(TokenType::StatementSeparator));
    }

    #[test]
    fn it_parses_a_simple_assignment() {
        // Corresponds to the following source code:
        // x=3;
        let mut tokens: Vec<Token> = vec!(
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

        tokens.iter_mut().for_each(|token| ast.parse_token(token));
        ast.build_and_run(tokens);

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
        let mut tokens: Vec<Token> = vec!(
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

        tokens.iter_mut().for_each(|token| ast.parse_token(token));
        ast.build_and_run(tokens);

        assert_eq!(ast.scope.get("x"), Some(&Number(4)));
    }

    #[test]
    fn it_can_assign_a_variable_to_another_variable() {
        let mut ast = AST::new();
        // Corresponds to the following source code:
        // x=3;x=4;x=y;
        let mut tokens: Vec<Token> = vec!(
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

        tokens.iter_mut().for_each(|token| ast.parse_token(token));
        ast.build_and_run(tokens);

        assert_eq!(ast.scope.get("x"), Some(&Number(4)));
        assert_eq!(ast.scope.get("y"), Some(&Number(4)));
    }

    #[test]
    fn it_can_assign_a_new_value_after_being_assigned_to_another_variable() {
        // Corresponds to the following source code:
        // x=3;x=4;x=y;x=5;
        let mut tokens: Vec<Token> = vec!(
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
        tokens.iter_mut().for_each(|token| ast.parse_token(token));
        ast.build_and_run(tokens);

        assert_eq!(ast.scope.get("x"), Some(&Number(5)));
        assert_eq!(ast.scope.get("y"), Some(&Number(4)));
    }
}