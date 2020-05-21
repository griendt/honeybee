use ansi_term::Colour::{Green, Red, Yellow};

pub fn info(message: &str) {
    println!("{}", Green.bold().paint(message));
}

pub fn warn(message: &str) {
    println!("{}", Yellow.bold().paint(message));
}

pub fn error(message: &str) {
    println!("{}", Red.bold().paint(message));
}
