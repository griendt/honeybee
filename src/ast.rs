use crate::token::{Token, TokenCategory, TokenType};
use std::borrow::Borrow;
use crate::io::{info, error};
use std::collections::HashMap;

#[derive(Debug)]
pub struct AST {}

#[derive(Debug)]
pub enum HoneyValue {
    Number(u64),
}

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

    pub fn execute(&self, scope: &mut HashMap<String, HoneyValue>) {
        // We should probably require a context or scope for the statement as an argument.
        // Right now we won't care about that and just execute whatever is inside the statement
        // as if it were the only part of the entire program.
        match self._type {
            Some(StatementType::Assignment) => self.execute_assignment(scope),

            // We could, in theory, determine the type on the go, but this should never
            // be needed. Therefore we will explicitly panic because it implies bad
            // implementation at some point during the parsing of the tokens.
            None => panic!("Cannot execute statement whose type was not evaluated"),

            _ => panic!("Unsupported statement type: {:?}", self._type)
        }
    }

    fn execute_assignment(&self, scope: &mut HashMap<String, HoneyValue>) {
        // An assignment consists of two parts: the left side, which should be an identifier,
        // and the right side, which should be an expression that reduces to a single value.
        // For now we only support elementary identifiers on the left side (i.e. one token).
        // We should extend this, of course, to implement assignments such as foo.bar = baz.
        // However, this extension COULD be implemented in a part of code that gets executed
        // before this function is reached.
        assert_eq!(self.tokens[0]._type, Some(TokenType::VariableName));
        assert_eq!(self.tokens[1]._type, Some(TokenType::AssignmentOperator));

        error("We are going to execute an assignment!");
        // This is probably not the best check to ask whether we are dealing with an elementary
        // expression... the left- and right hand sides should probably already be split before
        // we even reach this part of the code, and we should ask of each side whether they are
        // elementary (and if not, execute the sub-expressions first). But right now we are only
        // supporting elementary expressions.
        if self.tokens.len() == 4 {
            let value = self.tokens[2].to_honey_value();
            scope.insert(self.tokens[0].value.clone(), value);

            println!("Scope is now:");
            println!("{:#?}", scope);
        }


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

    // T is the generic that will be the kind of values that exist in the language.
    // We should add a trait that this T should implement, and make sure to typehint
    // that trait(s) in the functions that this function calls as well.
    pub fn make<T>(&self, tokens: &mut Vec<Token>) {
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

        let global_scope: &mut HashMap<String, HoneyValue> = &mut HashMap::new();
        // Technically we could already execute the code as we are parsing the next parts
        // of the code. However this is quite dangerous (a file should not execute at all if
        // it contains any syntax errors) and requires threading and so on... maybe one day.
        for statement in code_block.statements.iter() {
            statement.execute(global_scope);
        }

        info("Global state after executing:");
        println!("{:#?}", global_scope);
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