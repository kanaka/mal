use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::error::Error;
use std::fmt;

mod core;
mod env;
mod printer;
mod reader;
mod types;

use env::Env;
use types::MalType;

pub fn main() -> Result<(), Box<std::error::Error>> {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No history");
    }

    let mut env = Env::new(None);

    for (k, v) in core::ns() {
        env.set(k, &v);
    }

    rep("(def! not (fn* (a) (if a false true)))", &mut env);

    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_ref());
                rep(&line, &mut env);
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

fn rep(line: &str, env: &mut Env) {
    match read(&line) {
        Err(err) => println!("{}", err),
        Ok(r) => match eval(&r, env) {
            Ok(e) => {
                print(&e);
            }
            Err(err) => {
                println!("{}", err);
            }
        },
    }
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

fn eval_ast(ast: &MalType, env: &mut Env) -> types::Result {
    match ast {
        MalType::Symbol(s) => env.get(s),
        MalType::List(lst) => match lst.iter().map(|item| eval(item, env)).collect() {
            Ok(res) => Ok(MalType::List(res)),
            Err(err) => Err(err),
        },
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
        _ => Ok(ast.clone()),
    }
}

fn eval_def(l: &[MalType], env: &mut Env) -> types::Result {
    if let MalType::Symbol(key) = &l[0] {
        eval(&l[1], env).map(|val| {
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
    if l.len() < 2 {
        return EvalError::new("to few parameters for let*");
    }
    let mut new_env = Env::new(Some(env));

    if let Some(bindings) = get_bindings_as_list(&l[0]) {
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

    eval(&l[1], &mut new_env)
}

fn eval_do(l: &[MalType], env: &mut Env) -> types::Result {
    match eval_ast(&MalType::List(l.to_vec()), env) {
        Ok(MalType::List(ref v)) => Ok(v[v.len() - 1].clone()),
        o => o,
    }
}

fn eval_if(l: &[MalType], env: &mut Env) -> types::Result {
    if l.len() < 2 {
        return EvalError::new("if requires three parameters");
    }
    let e_res = eval(&l[0], env)?;
    match e_res {
        MalType::Nil | MalType::Boolean(false) => {
            if l.len() > 2 {
                eval(&l[2], env)
            } else {
                Ok(MalType::Nil)
            }
        }
        _ => eval(&l[1], env),
    }
}

fn eval_fn(l: &[MalType], env: &mut Env) -> types::Result {
    if l.len() < 2 {
        return EvalError::new("fn* requires two parameters");
    }

    let o_binds = match &l[0] {
        MalType::List(binds) => Some(binds.clone()),
        MalType::Vector(binds) => Some(binds.clone()),
        _ => None,
    };

    let old_env = env.clone();

    let lambda = l[1].clone();

    if let Some(binds) = o_binds {
        Ok(MalType::Fn(std::sync::Arc::new(move |exprs, outer_env| {
            let mut new_env = Env::new_with_binds(Some(&outer_env), &binds, exprs);
            new_env.set_inner(&old_env);
            let res = eval(&lambda, &mut new_env);
            res
        })))
    } else {
        EvalError::new("fn* requires two list parameters")
    }
}

fn eval(ast: &MalType, env: &mut Env) -> types::Result {
    match ast {
        MalType::List(l) if l.len() == 0 => Ok(ast.clone()),
        MalType::List(l) => match &l[0] {
            MalType::Symbol(ref s) if s == "def!" => eval_def(&l[1..], env),
            MalType::Symbol(ref s) if s == "let*" => eval_let(&l[1..], env),
            MalType::Symbol(ref s) if s == "do" => eval_do(&l[1..], env),
            MalType::Symbol(ref s) if s == "if" => eval_if(&l[1..], env),
            MalType::Symbol(ref s) if s == "fn*" => eval_fn(&l[1..], env),
            _ => match eval_ast(ast, env) {
                Ok(MalType::List(elist)) => match &elist[0] {
                    MalType::Fn(f) => f(&elist[1..], env),
                    _ => EvalError::new("Missing function when evaluating list"),
                },
                Ok(_) => EvalError::new("Unknown error when evaluating list"),
                err => err,
            },
        },
        _ => eval_ast(ast, env),
    }
}

fn print(s: &types::MalType) {
    println!("{}", printer::pr_str(s, true));
}
