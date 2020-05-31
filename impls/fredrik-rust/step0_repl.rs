use rustyline::error::ReadlineError;
use rustyline::Editor;
pub fn main() -> Result<(), Box<std::error::Error>> {
    let mut rl = Editor::<()>::new();
    rl.load_history("history.txt");

    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_ref());
                println!("{}", line);
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
