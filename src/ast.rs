use std::borrow::Borrow;
use std::collections::HashMap;
use std::option::Option::None;

use crate::ast::StatementContent::Assignment;
use crate::io::{error, info};
use crate::token::{Token, TokenCategory, TokenType};
use crate::token::TokenType::AssignmentOperator;
use std::process::exit;

#[derive(Debug)]
pub struct AST {
    pub scope: HashMap<String, HoneyValue>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum HoneyValue {
    Number(u64),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CodeBlock {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentStatement {
    // We should expand on this to allow multi-token variable assignment,
    // such as foo.bar
    variable: String,

    value: Box<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CodeBlockStatement {
    statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementContent {
    // These contents are leaves (no sub-statements)
    None,
    Leaf(LeafStatement),

    // These contents have sub-statements
    Assignment(AssignmentStatement),
    CodeBlock(CodeBlockStatement),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LeafStatement {
    VariableName(String),
    HoneyValue(HoneyValue),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub tokens: Vec<Token>,
    pub content: StatementContent,
}

impl Statement {
    pub fn from_tokens(tokens: Vec<Token>) -> Self {
        let mut statement = Statement { tokens: vec![], content: StatementContent::None };
        statement.tokens = tokens;
        statement.set_content();
        statement
    }

    pub fn set_content(&mut self) -> () {
        // This is not a great check: we turn a statement into an assignment if it contains
        // an assignment operator. This, of course, doesn't work well in case this operator
        // has a lower precedence than some other (for example because it's in parantheses).
        match self.tokens
            .iter()
            .position(|token| token._type == Some(AssignmentOperator))
        {
            Some(operator_position) => {
                // This is actually a bad assertion: only simple expressions pass this (e.g.
                // assignments where the left hand side is a single token). But for now this
                // is the only kind of assignment we will support.
                assert_eq!(operator_position, 1);

                // Make sure that the left hand side is, indeed, a variable and not something
                // else (such as a literal). Throw a syntax error otherwise.
                if self.tokens[0].category == TokenCategory::Literal {
                    self.tokens[0].print_error(String::from("Syntax error: Cannot assign to literal"));
                    exit(1);
                }

                self.content = Assignment(AssignmentStatement {
                    variable: self.tokens[0].value.clone(),
                    value: Box::from(
                        Statement::from_tokens(
                            self.tokens.clone()[operator_position + 1..].to_owned()
                        )
                    ),
                });
            },
            None => {
                // A naive check to look at whether we are dealing with a singular value:
                // a token and a semicolon. We should improve this.
                if self.tokens.len() == 2 {
                    self.content = match self.tokens[0].category {
                        TokenCategory::Identifier => StatementContent::Leaf(
                            LeafStatement::VariableName(self.tokens[0].value.clone())
                        ),
                        TokenCategory::Literal => {
                            StatementContent::Leaf(
                                LeafStatement::HoneyValue(
                                    // Here we assume right now that Number is the only possible
                                    // literal value. Therefore we try to parse it as a number.
                                    // Of course we should make an additional check here on the
                                    // type of the literal and create the appropriate HoneyValue!
                                    match self.tokens[0].value.parse::<u64>() {
                                        Ok(x) => HoneyValue::Number(x),
                                        Err(_) => panic!("Could not cast '{}' to u64", self.tokens[0].value),
                                    }
                                )
                            )
                        }
                        _ => panic!(
                            "Unprocessable token category found in single-token statement: '{:#?}'",
                            self.tokens[0].category
                        )
                    }
                }
            }
        }
    }

    pub fn parse(&mut self) {
        match self.content.borrow() {
            // Leaves need no further parsing
            StatementContent::Leaf(_) => {},
            StatementContent::CodeBlock(code_block_statement) => {
                let mut code_block_statement = code_block_statement.clone();
                for statement in code_block_statement.statements.iter_mut() {
                    statement.parse();
                }

                self.content = StatementContent::CodeBlock(code_block_statement);
            },
            StatementContent::Assignment(assignment_statement) => {
                // Assignments have a value (=r.h.s.) which is composed of a statement on its own.
                // We should parse that statement as well.
                let mut assignment_statement = assignment_statement.clone();
                assignment_statement.value.parse();
                self.content = StatementContent::Assignment(assignment_statement);
            },
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
                for statement in code_block_statement.statements {
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
            StatementContent::Assignment(_) => self.execute_assignment(scope),

            // The var is not on the left side of an assignment, because if it were, it would have
            // been consumed in the execute_assignment function. Therefore we should be able
            // to expand the variable to its value.
            StatementContent::Leaf(LeafStatement::VariableName(name)) =>
                match scope.get(name) {
                Some(x) => Ok(Some(x.clone())),
                None => {
                    self.tokens[0].print_error(format!("Undefined variable: '{}'", name));
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
            4 => Ok(Some(self.tokens[2].to_honey_value(scope))),
            _ => {
                Statement::from_tokens(self.tokens[2..].to_owned())
                    .execute(scope)
            }
        };

        match value {
            Ok(_) => {
                let value_to_insert = value.borrow().as_ref().unwrap().unwrap();
                scope.insert(self.tokens[0].value.clone(), value_to_insert);
                value
            }
            Err(_) => value,
        }
    }

    // A CodeBlock-type statement contains sub-statements.
    // This method splits that statement into a vector of its sub-statements,
    // and sets the content to these statements.
    fn set_statements_from_code_block(&mut self) -> () {
        match self.content {
            StatementContent::CodeBlock(_) => (),
            _ => panic!("Trying to parse a non-block statement as a code block"),
        };

        let mut statements: Vec<Statement> = vec![];
        let statement = &mut Statement { tokens: vec![], content: StatementContent::None };
        for token in self.tokens.iter() {
            statement.tokens.push(token.clone());
            if token._type == Some(TokenType::StatementSeparator) {
                statement.set_content();
                statements.push((&*statement).clone());
                statement.tokens = vec!();
            }
        }

        self.content = StatementContent::CodeBlock(CodeBlockStatement { statements });
    }
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

    pub fn build_and_run(&mut self, tokens: Vec<Token>) -> () {
        // Any program is really a code block. So, we will create a CodeBlock
        // type statement as the root node of the abstract syntax tree.
        // Then we will parse the code block, which in turn will parse its sub-statements.
        let code_block = &mut Statement {
            tokens: tokens.clone(),
            content: StatementContent::CodeBlock(CodeBlockStatement { statements: vec![] }),
        };

        code_block.set_statements_from_code_block();
        code_block.parse();

        info("Generated AST successfully.");

        let result = code_block.execute(&mut self.scope);
        match result {
            Ok(Some(value)) => info(format!(
                "Program executed successfully with return value: {:?}", value
            ).as_str()),
            Ok(None) => info("Program executed successfully with no return value"),
            Err(e) => error(format!("Program failed with error: {}", e).as_str())
        };
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
        ast.build_and_run(tokens);

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
        ast.build_and_run(tokens);

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
        ast.build_and_run(tokens);

        assert_eq!(ast.scope.get("x"), Some(&Number(5)));
        assert_eq!(ast.scope.get("y"), Some(&Number(4)));
    }
}