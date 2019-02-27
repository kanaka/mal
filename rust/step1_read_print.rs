#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate itertools;
extern crate fnv;

extern crate rustyline;
use rustyline::error::ReadlineError;
use rustyline::Editor;

#[macro_use]
#[allow(dead_code)]
mod types;
use types::{format_error};
mod reader;
mod printer;
// TODO: figure out a way to avoid including env
#[allow(dead_code)]
mod env;

fn main() {
  // `()` can be used when no completer is required
  let mut rl = Editor::<()>::new();
  if rl.load_history(".mal-history").is_err() {
      println!("No previous history.");
  }

  loop {
    let readline = rl.readline("user> ");
    match readline {
      Ok(line) => {
        rl.add_history_entry(&line);
        rl.save_history(".mal-history").unwrap();
        if line.len() > 0 {
          match reader::read_str(line) {
            Ok(mv) => {
              println!("{}", mv.pr_str(true));
            },
            Err(e)  => println!("Error: {}", format_error(e)),
          }
        }
      },
      Err(ReadlineError::Interrupted) => continue,
      Err(ReadlineError::Eof) => break,
      Err(err) => {
        println!("Error: {:?}", err);
        break
      }
    }
  }
}

// vim: ts=2:sw=2:expandtab
