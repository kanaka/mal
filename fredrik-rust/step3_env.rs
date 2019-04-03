use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::error::Error;
use std::fmt;

mod env;
mod printer;
mod reader;
mod types;

use env::Env;
use types::MalType;

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

    let mut env = Env::new(None);
    env.set("+", &MalType::Fn(std::sync::Arc::new(add)));
    env.set("-", &MalType::Fn(std::sync::Arc::new(sub)));
    env.set("*", &MalType::Fn(std::sync::Arc::new(mul)));
    env.set("/", &MalType::Fn(std::sync::Arc::new(div)));

    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_ref());
                match read(&line) {
                    Err(err) => println!("{}", err),
                    Ok(r) => match eval(&r, &mut env) {
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

fn eval_ast(v: &MalType, env: &mut Env) -> types::Result {
    match v {
        MalType::Symbol(s) => env.get(s),
        MalType::List(lst) => match lst.iter().map(|item| eval(item, env)).collect() {
            Ok(res) => Ok(MalType::List(res)),
            Err(err) => Err(err),
        },
        _ => Ok((*v).clone()),
    }
}

fn eval_def(l: &[MalType], env: &mut Env) -> types::Result {
    if let MalType::Symbol(key) = &l[1] {
        eval(&l[2], env).map(|val| {
            env.set(&key, &val);
            val
        })
    } else {
        EvalError::new("Failed to get key for def!")
    }
}

fn get_bindings_as_list(v: &MalType) -> Option<&Vec<MalType>> {
    match v {
        MalType::List(v) => Some(v),
        MalType::Vector(v) => Some(v),
        _ => None,
    }
}

fn eval_let(l: &[MalType], env: &mut Env) -> types::Result {
    if l.len() < 3 {
        return EvalError::new("to few parameters for let*");
    }
    let mut new_env = Env::new(Some(env));

    if let Some(bindings) = get_bindings_as_list(&l[1]) {
        if bindings.len() % 2 != 0 {
            return EvalError::new("Bindings list has to have an even number of parameters");
        }
        for i in (0..bindings.len()).step_by(2) {
            if let MalType::Symbol(key) = &bindings[i] {
                match eval(&bindings[i + 1], &mut new_env) {
                    Ok(eval_val) => {
                        new_env.set(&key, &eval_val);
                    }
                    err => {
                        return err;
                    }
                }
            } else {
                return EvalError::new("Key in let binding have to be a symbol");
            }
        }
    } else {
        return EvalError::new("Bindings have to be a list");
    }

    eval(&l[2], &mut new_env)
}

fn eval(v: &MalType, env: &mut Env) -> types::Result {
    match v {
        MalType::List(l) => {
            if l.len() == 0 {
                Ok((*v).clone())
            } else {
                match &l[0] {
                    MalType::Symbol(s) => match s.as_str() {
                        "def!" => eval_def(&l, env),
                        "let*" => eval_let(&l, env),
                        _ => match eval_ast(v, env) {
                            Ok(MalType::List(elist)) => match &elist[0] {
                                MalType::Fn(f) => f(&elist[1..]),
                                _ => EvalError::new("Missing function when evaluating list"),
                            },
                            Ok(_) => EvalError::new("Unknown error when evaluating list"),
                            err => err,
                        },
                    },
                    _ => EvalError::new("Unknow first lsit elemenet"),
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
    println!("{}", printer::pr_str(s, true));
}
