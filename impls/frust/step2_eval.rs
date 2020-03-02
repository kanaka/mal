const HISTORY: &str = "history.txt";

mod common;
mod core;
mod env;
mod printer;
mod reader;
mod types;

pub use common::*;
use types::MalType;

fn main() {
    let rl_config = rustyline::Config::builder()
        .auto_add_history(true)
        .completion_type(rustyline::CompletionType::Circular)
        .edit_mode(rustyline::EditMode::Vi)
        .color_mode(rustyline::ColorMode::Enabled)
        .build();
    let mut rl = rustyline::Editor::<()>::with_config(rl_config);
    let _ = rl.load_history(HISTORY);

    let mut env = env::Env::new();

    for readline in rl.iter("user> ") {
        match readline {
            Ok(s) => {
                match rep(s, &mut env) {
                    Ok(s) => println!("{}", s),
                    Err(e) => eprintln!("{}", e),
                };
            }
            Err(e) => eprintln!("{}", e),
        }
    }
    rl.save_history(HISTORY).expect("Failed to save history");
}

fn rep(s: String, env: &mut env::Env) -> Result<String> {
    let r = read(s)?;
    let e = eval(r, env)?;
    Ok(print(e))
}

fn read(s: String) -> MalResult {
    reader::read_str(s)
}

fn eval(t: types::MalType, env: &mut env::Env) -> MalResult {
    match &t {
        MalType::List(l, _) if l.is_empty() => Ok(t),
        MalType::List(_, _) => {
            let new_l = eval_ast(t, env)?;
        }
        _ => eval_ast(t, env),
    }
}

fn eval_ast(t: types::MalType, env: &mut env::Env) -> MalResult {
    match t {
        MalType::Symbol(ref s) => env.lookup(s),
        MalType::List(lst, meta) => {
            let mut l = Vec::new();
            for v in lst {
                let new_v = Box::new(eval(*v, env)?);
                l.push(new_v);
            }
            Ok(MalType::List(l, meta))
        }
        any => Ok(any),
    }
}

fn print(t: types::MalType) -> String {
    printer::pr_str(&t, true)
}
