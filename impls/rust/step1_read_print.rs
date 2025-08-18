#![allow(non_snake_case)]

extern crate fnv;
extern crate itertools;
extern crate regex;

mod readline;
#[macro_use]
#[allow(dead_code)]
mod types;
use crate::types::{MalRet, MalVal};
#[allow(dead_code)]
mod env;
mod printer;
mod reader;

// read
fn read(str: &str) -> MalRet {
    reader::read_str(str)
}

// eval
fn eval(ast: MalVal) -> MalRet {
    Ok(ast)
}

// print
fn print(ast: MalVal) -> String {
    ast.pr_str(true)
}

fn rep(str: &str) -> Result<String, MalVal> {
    let ast = read(str)?;
    let exp = eval(ast)?;
    Ok(print(exp))
}

fn main() {
    // `()` can be used when no completer is required

    // main repl loop
    while let Some(ref line) = readline::readline("user> ") {
        if !line.is_empty() {
            match rep(line) {
                Ok(ref out) => println!("{}", out),
                Err(ref e) => println!("Error: {}", e.pr_str(true)),
            }
        }
    }
    println!();
}
