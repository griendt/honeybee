use crate::ast::{Statement, StatementContent};
use crate::io::info;

const OPCODES: [&str; 2] = [
    "DECLARE",
    "RETURN"
];

pub(crate) struct BytecodeGenerator {}

impl BytecodeGenerator {
    pub fn generate(root: &Statement) -> Vec<String> {
        let mut commands: Vec<String> = vec![];

        let statements = match &root.content {
           StatementContent::CodeBlock(statements) => statements,
            _ => panic!("Cannot generate bytecode for a non-code-block statement"),
        };

        let mut last_value = 0u64;
        for statement in statements {
            match &statement.content {
                StatementContent::None => (),
                StatementContent::Declaration(var_name, var_value) => {
                    // Right now we will assume the right hand side is already completely
                    // flattened to a single value. We will support more complex right hand
                    // sides later.

                    match &var_value.content {
                        StatementContent::Leaf(x) => {
                            commands.push(format!("DECLARE {} {}", var_name, x));
                            last_value = x.parse::<u64>().unwrap();
                        },
                        _ => unimplemented!("Right hand side must be a simple value (leaf)")
                    }
                },
                _ => unimplemented!("No implementation yet for statement content type {:#?}", statement.content)
            }
        }

        // The return value is the evaluation value of the last explicitly given command.
        // We will (probably) also support RETURN VARIABLE in the future so that we can
        // tell LLVM to build a return value from a reference appropriately.
        commands.push(format!("RETURN_VALUE {}", last_value));

        info("Generated opcodes:");
        println!("{:#?}", commands);
        commands
    }
}