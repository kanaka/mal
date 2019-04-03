use rustyline::error::ReadlineError;
use rustyline::Editor;

mod printer;
mod reader;
mod types;

pub fn main() -> Result<(), Box<std::error::Error>> {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No history");
    }

    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_ref());
                match read(&line) {
                    Err(err) => println!("{}", err),
                    Ok(r) => {
                        let e = eval(&r);
                        print(e);
                    }
                }
            }
            Err(ReadlineError::Eof) => break,
            Err(ReadlineError::Interrupted) => continue,
            Err(err) => {
                eprintln!("Readline error: {}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt")?;
    Ok(())
}

fn read(str: &str) -> types::Result {
    reader::read_str(str)
}

fn eval(str: &types::MalType) -> &types::MalType {
    str
}

fn print(s: &types::MalType) {
    println!("{}", printer::pr_str(s, false));
}
