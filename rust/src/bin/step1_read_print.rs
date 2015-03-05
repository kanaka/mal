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

fn rep(str: &str) -> Result<String,MalError> {
    let ast = try!(read(str.to_string()));
    //println!("read: {}", ast);
    let exp = try!(eval(ast));
    Ok(print(exp))
}

fn main() {
    loop {
        let line = readline::mal_readline("user> ");
        match line { None => break, _ => () }
        match rep(&line.unwrap()) {
            Ok(str)  => println!("{}", str),
            Err(ErrMalVal(_)) => (),  // Blank line
            Err(ErrString(s)) => println!("Error: {}", s),
        }
    }
}
