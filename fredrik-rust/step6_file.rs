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
        Ok(r) => match eval(r, env) {
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
        MalType::List(lst) => match lst.iter().map(|item| eval(item.clone(), env)).collect() {
            Ok(res) => Ok(MalType::List(res)),
            Err(err) => Err(err),
        },
        MalType::Vector(l) => match l.iter().map(|i| eval(i.clone(), env)).collect() {
            Ok(res) => Ok(MalType::Vector(res)),
            Err(err) => Err(err),
        },
        MalType::HashMap(hm) => {
            let mut new = Vec::new();
            for i in (0..hm.len()).step_by(2) {
                new.push(hm[i].clone());
                match eval(hm[i + 1].clone(), env) {
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
        eval(l[1].clone(), env).map(|val| {
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

fn eval_let(l: &[MalType], env: &mut Env) -> std::result::Result<(MalType, Env), Box<Error>> {
    if l.len() < 2 {
        return Err(Box::new(EvalError {
            err: "too few parameters for let*".to_owned(),
        }));
    }
    let mut new_env = Env::new(Some(env));

    if let Some(bindings) = get_bindings_as_list(&l[0]) {
        if bindings.len() % 2 != 0 {
            return Err(Box::new(EvalError {
                err: "let* bindings list has to have an even number of parameters".to_owned(),
            }));
        }
        for i in (0..bindings.len()).step_by(2) {
            if let MalType::Symbol(key) = &bindings[i] {
                match eval(bindings[i + 1].clone(), &mut new_env) {
                    Ok(eval_val) => {
                        new_env.set(&key, &eval_val);
                    }
                    Err(err) => {
                        return Err(err);
                    }
                }
            } else {
                return Err(Box::new(EvalError {
                    err: "let* bindings list keys have to be symbols".to_owned(),
                }));
            }
        }
    } else {
        return Err(Box::new(EvalError {
            err: "let* bindings has to be a list".to_owned(),
        }));
    }

    Ok((l[1].clone(), new_env))
}

fn eval_if(l: &[MalType], env: &mut Env) -> types::Result {
    if l.len() < 2 {
        return EvalError::new("if requires three parameters");
    }
    let e_res = eval(l[0].clone(), env)?;
    match e_res {
        MalType::Nil | MalType::Boolean(false) => {
            if l.len() > 2 {
                Ok(l[2].clone())
            } else {
                Ok(MalType::Nil)
            }
        }
        _ => Ok(l[1].clone()),
    }
}

fn eval_fn(fun: &MalType, l: &[MalType], env: &mut Env) -> types::Result {
    if l.len() < 2 {
        return EvalError::new("fn* requires two parameters");
    }

    let params = match &l[0] {
        MalType::List(p) => Some(p.clone()),
        MalType::Vector(p) => Some(p.clone()),
        _ => None,
    }
    .ok_or(EvalError {
        err: "Params to fn has to be a list or a vector".to_owned(),
    })?;

    let ast = l[1].clone();

    Ok(MalType::MalFunc(
        Box::new(ast),
        params,
        env.clone(),
        Box::new(fun.clone()),
    ))
}

fn eval(input_ast: MalType, env: &mut Env) -> types::Result {
    let mut ast = input_ast;
    loop {
        match ast {
            MalType::List(ref l) if l.len() == 0 => return Ok(ast.clone()),
            MalType::List(ref l) => match &l[0] {
                MalType::Symbol(ref s) if s == "def!" => return eval_def(&l[1..], env),
                MalType::Symbol(ref s) if s == "let*" => {
                    let (a, e) = eval_let(&l[1..], env)?;
                    ast = a;
                    *env = e;
                }
                MalType::Symbol(ref s) if s == "do" => {
                    // ast = eval_do(&l[1..], env)?;
                    let mut do_env = env.clone();
                    for i in 1..(l.len() - 1) {
                        eval_ast(&l[i], &mut do_env)?;
                    }
                    ast = l[l.len() - 1].clone();
                }
                MalType::Symbol(ref s) if s == "if" => {
                    ast = eval_if(&l[1..], env)?;
                }
                MalType::Symbol(ref s) if s == "fn*" => return eval_fn(&ast, &l[1..], env),
                _ => match eval_ast(&ast, env) {
                    Ok(MalType::List(elist)) => match &elist[0] {
                        MalType::Fn(f) => return f(&elist[1..], env),
                        MalType::MalFunc(f_ast, params, f_env, fun) => {
                            // println!("elist: {:?}", &elist[1]);
                            *env = Env::new_with_binds(Some(env), params, &elist[1..]);
                            ast = *f_ast.clone();
                        }
                        _ => return EvalError::new("Missing function when evaluating list"),
                    },
                    Ok(_) => return EvalError::new("Unknown error when evaluating list"),
                    err => return err,
                },
            },
            _ => return eval_ast(&ast, env),
        }
    }
}

fn print(s: &types::MalType) {
    println!("{}", printer::pr_str(s, true));
}
