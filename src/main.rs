#![feature(core_panic)]

use std::env;

mod parser;
mod lexer;
mod io;
mod token;
mod ast;

fn run(file: String) {
    parser::parse(file);
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut first_arg = true;

    for arg in args {
        if first_arg {
            // The first argument is the executable itself
            first_arg = false;
            continue;
        }

        run(arg);
    }

}
