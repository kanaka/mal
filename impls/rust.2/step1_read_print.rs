extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

mod reader;
mod types;
mod printer;

fn read(input: String) -> Option<crate::types::MalValue> {
    return reader::Reader::read_str(input.to_string());
}

fn eval(input: crate::types::MalValue) -> crate::types::MalValue {
    return input;
}

fn print(input: crate::types::MalValue) -> String {
    return crate::printer::pr_str(input);
}

fn rep(input: String) -> String {
    let ast = read(input);
    match ast {
        None => return String::default(),
        Some(a) => {
            let result = eval(a);
            return print(result);
        }
    }    
}


fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history(".mal-history").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                println!();
                println!("{}", rep(line));
            },
            Err(ReadlineError::Interrupted) => {
                break
            },
            Err(ReadlineError::Eof) => {
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
}