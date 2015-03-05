extern crate mal;

use mal::types::{MalVal, MalRet, MalError};
use mal::types::MalError::{ErrString, ErrMalVal};
use mal::{readline, reader};

// read
fn read(str: String) -> MalRet {
    reader::read_str(str)
}

// eval
fn eval(ast: MalVal) -> MalRet {
    Ok(ast)
}

// print
fn print(exp: MalVal) -> String {
    exp.pr_str(true)
}

fn rep(str: String) -> Result<String,MalError> {
    match read(str) {
        Err(e) => Err(e),
        Ok(ast) => {
            //println!("read: {}", ast);
            match eval(ast) {
                Err(e)  => Err(e),
                Ok(exp) => Ok(print(exp)),
            }
        }
    }
}

fn main() {
    loop {
        let line = readline::mal_readline("user> ");
        match line { None => break, _ => () }
        match rep(line.unwrap()) {
            Ok(str)  => println!("{}", str),
            Err(ErrMalVal(_)) => (),  // Blank line
            Err(ErrString(s)) => println!("Error: {}", s),
        }
    }
}
