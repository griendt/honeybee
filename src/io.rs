use ansi_term::Colour::{Green, Red, Yellow};

pub fn info(message: &str) {
    print!("{}", Green.bold().paint(message));
}

pub fn warn(message: &str) {
    print!("{}", Yellow.bold().paint(message));
}

pub fn error(message: &str) {
    print!("{}", Red.bold().paint(message));
}
