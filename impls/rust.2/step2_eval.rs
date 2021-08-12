extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

mod reader;
mod types;
mod printer;
mod env;

fn read(input: String) -> Result<Option<crate::types::MalValue>, crate::types::MalError> {
    return reader::Reader::read_str(input.to_string());
}

fn eval(input: crate::types::MalValue, _env: &env::Environment) -> crate::types::MalValue {
    return input;
}

fn print(input: crate::types::MalValue) -> String {
    return crate::printer::pr_str(input, true);
}

fn rep(input: String, env: &env::Environment) -> String {
    let ast = read(input);
    match ast {
        Err(e) => {
            return format!("Error! {:?}", e);
        }
        Ok(v) => {
            match v {
                None => return String::default(),
                Some(a) => {
                    let result = eval(a, env);
                    return print(result);
                }
            }
        }
    }    
}


fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history(".mal-history").is_err() {
        println!("No previous history.");
    }

    let env = env::Environment{};

    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let result = rep(line.trim_end().to_string(), &env);
                print!("{}", result);
                println!();
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