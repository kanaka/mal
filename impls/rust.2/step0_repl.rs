#![allow(non_snake_case)]

fn READ(input: &str) -> &str {
    input
}

fn EVAL(input: &str) -> &str {
    input
}

fn PRINT(input: &str) -> &str {
    input
}

fn rep(input: &str) -> &str {
    let read_result = READ(input);
    let eval_result = EVAL(read_result);
    PRINT(eval_result)
}

fn main() {
    let mut rl = rustyline::Editor::<()>::new();
    let _result = rl.load_history(" history.txt");
    loop {
        let readline = rl.readline("user> ");

        match readline {
            Ok(input) => {
                let result = rep(&input);
                println!("{}", result);
                rl.add_history_entry(input);
            }
            Err(_) => break,
        }
    }
    rl.save_history("history.txt").unwrap();
}
