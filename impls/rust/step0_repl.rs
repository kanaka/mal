#![allow(non_snake_case)]

mod readline;

fn main() {
    // `()` can be used when no completer is required

    // main repl loop
    while let Some(ref line) = readline::readline("user> ") {
        if !line.is_empty() {
            println!("{}", line);
        }
    }
    println!();
}
