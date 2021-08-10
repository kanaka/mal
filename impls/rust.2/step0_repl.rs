extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

fn read(input: String) -> String {
    return input;
}

fn eval(input: String) -> String {
    return input;
}

fn print(input: String) -> String {
    return input;
}

fn rep(input: String) -> String {
    let a = read(input);
    let b = eval(a);
    let result = print(b);

    return result;
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