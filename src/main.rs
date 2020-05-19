#![feature(core_panic)]

use std::env;
use std::fs;

mod parser;
mod lexer;
mod io;
mod token;

fn run(code: String) {
    parser::parse(code);
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

        let contents = fs::read_to_string(arg)
            .expect("Something went wrong reading the file");

        run(contents);
    }

}
