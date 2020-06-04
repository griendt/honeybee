use std::collections::HashMap;
use std::error::Error;

use inkwell::attributes::AttributeLoc::Function;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::OptimizationLevel;
use inkwell::passes::PassManager;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::types::{FunctionType, StringRadix};
use inkwell::values::{FunctionValue, PointerValue, BasicValue, IntValue};
use crate::io::info;

struct Compiler<'ctx, 'a> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,

    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'ctx, 'a> Compiler<'ctx, 'a> {
    /// Returns the `FunctionValue` representing the function being compiled.
    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    /// Creates a new stack allocation instruction in the entry block of the function.
    fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry)
        }

        builder.build_alloca(self.context.i64_type(), name)
    }

    fn declaration(&mut self, var_name: String, value: u64) {
        match self.variables.get(var_name.as_str()) {
            Some(_) => println!("Redeclaring variable: {}", var_name),
            None => (),
        };

        let alloca = self.create_entry_block_alloca(var_name.as_str());
        let value = self.context.i64_type().const_int(value, false);
        self.builder.build_store(alloca, value);

        self.variables.insert(var_name, alloca);
    }

    fn retrieve(&self, var_name: String) -> Result<IntValue<'ctx>, &'static str> {
        match self.variables.get(var_name.as_str()) {
            Some(pointer) => Ok(
                self.builder.build_load(*pointer, var_name.as_str()).into_int_value()
            ),
            None => Err("Undefined variable"),
        }
    }
}

pub(crate) fn compile(bytecode: Vec<String>) -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("sum");
    let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;

    // Create FPM
    let fpm = PassManager::create(&module);
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.initialize();

    // Create main function. It returns an i64 and takes no arguments.
    let fn_type = context.i64_type().fn_type(&[], false);
    let function = module.add_function("__main__", fn_type, None);
    let basic_block = context.append_basic_block(function, "entry");
    let mut compiler = Compiler {
        context: &context,
        module: &module,
        builder: &context.create_builder(),
        variables: HashMap::new(),
        fpm: &fpm,
        fn_value_opt: Some(function),
    };

    compiler.builder.position_at_end(basic_block);

    // Right now we have no control flow so we can stick to one Basic Block.
    // Of course we will have to expand this in the future.
    for command in bytecode.iter() {
        let components: Vec<&str> = command.split(' ').collect();

        match components[0] {
            "DECLARE" => compiler.declaration(
                String::from(components[1]),
                components[2].parse::<u64>().unwrap()
            ),
            "RETURN_VALUE" => {
                // Note: We assume by force here that the return value is an integer.
                let i64_type = context.i64_type();
                let value = i64_type.const_int_from_string(
                    components[1],
                    StringRadix::Decimal
                ).unwrap();
                compiler.builder.build_return(Some(&value));
            },
            _ => {}
        };
    }

    // compiler.declaration(String::from("x"), 3);
    // compiler.retrieve(String::from("x"))
    //     .and_then(|result_value| {
    //         compiler.builder.build_return(Some(&result_value));
    //         Ok(())
    //     });

    info("Generated LLVM IR (before optimization):");
    function.print_to_stderr();

    compiler.fpm.run_on(&function);

    info("Generated LLVM IR (after optimization):");
    function.print_to_stderr();

    let maybe_fn = unsafe {
        execution_engine.get_function::<unsafe extern "C" fn() -> i64>("__main__")
    };
    let compiled_fn = match maybe_fn {
        Ok(f) => f,
        Err(err) => panic!("!> Error during execution: {:?}", err)
    };

    info("Compilation successful");

    unsafe {
        let program_result = compiled_fn.call();
        info("Program return value:");
        println!("{}", program_result);
    };

    Ok(())
}