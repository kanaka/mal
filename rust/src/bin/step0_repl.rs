extern crate mal;

use mal::readline::mal_readline;

// read
fn read(str: String) -> String {
    str
}

// eval
fn eval(ast: String) -> String {
    ast
}

// print
fn print(exp: String) -> String {
    exp
}

fn main() {
    loop {
        let line = mal_readline("user> ");
        match line { None => break, _ => () }
        println!("{}", print(eval(read(line.unwrap()))));
    }
}
