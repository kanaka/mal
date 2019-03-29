use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;

mod printer;
mod reader;
mod types;

use types::MalType;

type Env = HashMap<String, MalType>;

fn add(params: &[MalType]) -> types::Result {
    let mut sum = 0;
    for p in params {
        match p {
            MalType::Integer(i) => sum += i,
            _ => return EvalError::new("Can only sum numeric"),
        }
    }

    Ok(MalType::Integer(sum))
}

fn sub(params: &[MalType]) -> types::Result {
    if let MalType::Integer(init) = params[0] {
        let mut res = init;
        for p in params[1..].iter() {
            match p {
                MalType::Integer(i) => res -= i,
                _ => return EvalError::new("Can only sum numeric"),
            }
        }
        Ok(MalType::Integer(res))
    } else {
        EvalError::new("Can only sum numeric")
    }
}

fn mul(params: &[MalType]) -> types::Result {
    let mut res = 1;
    for p in params {
        match p {
            MalType::Integer(i) => res *= i,
            _ => return EvalError::new("Can only sum numeric"),
        }
    }

    Ok(MalType::Integer(res))
}

fn div(params: &[MalType]) -> types::Result {
    if let MalType::Integer(init) = params[0] {
        let mut res = init;
        for p in params[1..].iter() {
            match p {
                MalType::Integer(i) => res /= i,
                _ => return EvalError::new("Can only div numeric"),
            }
        }
        Ok(MalType::Integer(res))
    } else {
        EvalError::new("Can only div numeric")
    }
}

pub fn main() -> Result<(), Box<std::error::Error>> {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No history");
    }

    let mut env = Env::new();
    env.insert("+".to_string(), MalType::BIF(add));
    env.insert("-".to_string(), MalType::BIF(sub));
    env.insert("*".to_string(), MalType::BIF(mul));
    env.insert("/".to_string(), MalType::BIF(div));

    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_ref());
                match read(&line) {
                    Err(err) => println!("{}", err),
                    Ok(r) => match eval(&r, &env) {
                        Ok(e) => {
                            print(&e);
                        }
                        Err(err) => {
                            println!("{}", err);
                        }
                    },
                }
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

fn read(str: &str) -> types::Result {
    reader::read_str(str)
}

#[derive(Debug)]
struct EvalError {
    err: String,
}

impl EvalError {
    fn new(err: &str) -> types::Result {
        Err(Box::new(Self {
            err: err.to_owned(),
        }))
    }
}

impl Error for EvalError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Eval error: {}", self.err)
    }
}

fn eval_ast(v: &MalType, env: &Env) -> types::Result {
    match v {
        MalType::Symbol(s) => match env.get(s) {
            None => EvalError::new(&format!("Failed to lookup {}", s)),
            Some(val) => Ok((*val).clone()),
        },
        MalType::List(lst) => match lst.iter().map(|item| eval(item, env)).collect() {
            Ok(res) => Ok(MalType::List(res)),
            Err(err) => Err(err),
        },
        _ => Ok((*v).clone()),
    }
}

fn eval(v: &MalType, env: &Env) -> types::Result {
    match v {
        MalType::List(l) => {
            if l.len() == 0 {
                Ok((*v).clone())
            } else {
                match eval_ast(v, env) {
                    Ok(MalType::List(elist)) => match elist[0] {
                        MalType::BIF(f) => f(&elist[1..]),
                        _ => EvalError::new("Missing function when evaluating list"),
                    },
                    Ok(_) => EvalError::new("Unknown error when evaluating list"),
                    err => err,
                }
            }
        }
        MalType::Vector(l) => match l.iter().map(|i| eval(i, env)).collect() {
            Ok(res) => Ok(MalType::Vector(res)),
            Err(err) => Err(err),
        },
        MalType::HashMap(hm) => {
            let mut new = Vec::new();
            for i in (0..hm.len()).step_by(2) {
                new.push(hm[i].clone());
                match eval(&hm[i + 1], env) {
                    Ok(v) => new.push(v),
                    Err(err) => {
                        return Err(err);
                    }
                }
            }
            Ok(MalType::HashMap(new))
        }
        _ => eval_ast(v, env),
    }
}

fn print(s: &types::MalType) {
    println!("{}", printer::pr_str(s));
}
