use readline::mal_readline;
mod readline;

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
