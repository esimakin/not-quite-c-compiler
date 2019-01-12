mod emitter;
mod lexer;
mod parser;

use std::env;
use std::fs;
use std::io::prelude::*;
use std::process;

fn compile(contents: &String) -> Result<String, &'static str> {
    let tokens = lexer::tokenize(&contents);
    let program = parser::parse(tokens)?;
    let code = emitter::emit(program)?;
    Ok(code)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Source file is not provided.");
        process::exit(1);
    }

    let filename = &args[1];

    let contents = match fs::read_to_string(filename.clone()) {
        Ok(contents) => contents,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            process::exit(2);
        }
    };

    let mut out = String::from(&filename[0..filename.len() - 2]);
    out.push_str(".s");
    let mut file = match fs::File::create(out) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("Error creating output file: {}", e);
            process::exit(3);
        }
    };

    let contents = match compile(&contents) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Compile error: {}", e);
            process::exit(5);
        }
    };

    match file.write_all(contents.as_bytes()) {
        Ok(_) => (),
        Err(e) => {
            eprintln!("Error writing output file: {}", e);
            process::exit(4);
        }
    }
}
