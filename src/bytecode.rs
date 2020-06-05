use crate::ast::{Statement, StatementContent};
use crate::io::info;
use crate::token::TokenCategory;
use std::time::Instant;

enum ReturnValue {
    Void,
    Integer(u64),
    Variable(String),
}

pub(crate) struct BytecodeGenerator {}

impl BytecodeGenerator {
    pub fn generate(root: &Statement) -> Vec<String> {

        let start_time = Instant::now();
        let mut commands: Vec<String> = vec![];

        let statements = match &root.content {
           StatementContent::CodeBlock(statements) => statements,
            _ => panic!("Cannot generate bytecode for a non-code-block statement"),
        };

        let mut return_value = ReturnValue::Integer(0u64);

        for statement in statements {
            match &statement.content {
                StatementContent::None => (),
                StatementContent::Declaration(var_name, var_value) => {
                    // Right now we will assume the right hand side is already completely
                    // flattened to a single value. We will support more complex right hand
                    // sides later.

                    match &var_value.content {
                        StatementContent::Leaf(name, TokenCategory::Literal) => {
                            commands.push(format!("DECLARE {} VAL {}", var_name, name));
                            return_value = ReturnValue::Integer(name.parse::<u64>().unwrap());
                        },
                        StatementContent::Leaf(name, TokenCategory::Identifier) => {
                            commands.push(format!("DECLARE {} VAR {}", var_name, name));
                            return_value = ReturnValue::Variable(var_name.parse().unwrap());
                        }
                        _ => unimplemented!("Right hand side must be a simple value (leaf)")
                    }
                },
                _ => unimplemented!("No implementation yet for statement content type {:#?}", statement.content)
            }
        }

        // The return value is the evaluation value of the last explicitly given command.
        // We will (probably) also support RETURN VARIABLE in the future so that we can
        // tell LLVM to build a return value from a reference appropriately.
        match return_value {
            ReturnValue::Variable(name) => commands.push(format!("RETURN_VAR {}", name)),
            ReturnValue::Integer(value) => commands.push(format!("RETURN_VAL {}", value)),
            _ => unimplemented!("Do not know how to parse this return value type"),
        }

        info("Generated opcodes:");
        println!("{:#?}", commands);
        commands
    }
}