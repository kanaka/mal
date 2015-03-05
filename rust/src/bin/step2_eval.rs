extern crate mal;

use std::collections::HashMap;

use mal::types::{MalVal, MalRet, MalError, err_str};
use mal::types::{_nil, list, vector, hash_map, _int, func};
use mal::types::MalType::{Sym, List, Vector, Hash_Map, Int};
use mal::types::MalError::{ErrString, ErrMalVal};
use mal::{readline, reader};

// read
fn read(str: String) -> MalRet {
    reader::read_str(str)
}

// eval
fn eval_ast(ast: MalVal, env: &HashMap<String,MalVal>) -> MalRet {
    match *ast {
        Sym(ref sym) => {
            match env.get(sym) {
                Some(mv) => Ok(mv.clone()),
                None     => Ok(_nil()),
            }
        },
        List(ref a,_) | Vector(ref a,_) => {
            let mut ast_vec : Vec<MalVal> = vec![];
            for mv in a.iter() {
                match eval(mv.clone(), env) {
                    Ok(mv) => ast_vec.push(mv),
                    Err(e) => return Err(e),
                }
            }
            Ok(match *ast { List(_,_) => list(ast_vec),
                            _         => vector(ast_vec) })
        },
        Hash_Map(ref hm,_) => {
            let mut new_hm: HashMap<String,MalVal> = HashMap::new();
            for (key, value) in hm.iter() {
                match eval(value.clone(), env) {
                    Ok(mv) => { new_hm.insert(key.to_string(), mv); },
                    Err(e) => return Err(e),
                }
            }
            Ok(hash_map(new_hm))
        },
        _ => {
            Ok(ast.clone())
        }
    }
}

fn eval(ast: MalVal, env: &HashMap<String,MalVal>) -> MalRet {
    let ast2 = ast.clone();
    match *ast2 {
        List(_,_) => (),  // continue
        _ => return eval_ast(ast2, env),
    }

    // apply list
    match eval_ast(ast, env) {
        Err(e) => Err(e),
        Ok(el) => {
            match *el {
                List(ref args,_) => {
                    let ref f = args.clone()[0];
                    f.apply(args[1..].to_vec())
                }
                _ => err_str("Invalid apply"),
            }
        }
    }
}

// print
fn print(exp: MalVal) -> String {
    exp.pr_str(true)
}

fn rep(str: &str, env: &HashMap<String,MalVal>) -> Result<String,MalError> {
    match read(str.to_string()) {
        Err(e) => Err(e),
        Ok(ast) => {
            //println!("read: {}", ast);
            match eval(ast, env) {
                Err(e)  => Err(e),
                Ok(exp) => Ok(print(exp)),
            }
        }
    }
}

fn int_op<F>(f: F, a:Vec<MalVal>) -> MalRet
    where F: FnOnce(isize, isize) -> isize
{
    match *a[0] {
        Int(a0) => match *a[1] {
            Int(a1) => Ok(_int(f(a0,a1))),
            _ => err_str("second arg must be an int"),
        },
        _ => err_str("first arg must be an int"),
    }
}
fn add(a:Vec<MalVal>) -> MalRet { int_op(|i,j| { i+j }, a) }
fn sub(a:Vec<MalVal>) -> MalRet { int_op(|i,j| { i-j }, a) }
fn mul(a:Vec<MalVal>) -> MalRet { int_op(|i,j| { i*j }, a) }
fn div(a:Vec<MalVal>) -> MalRet { int_op(|i,j| { i/j }, a) }

fn main() {
    let mut repl_env : HashMap<String,MalVal> = HashMap::new();
    repl_env.insert("+".to_string(), func(add));
    repl_env.insert("-".to_string(), func(sub));
    repl_env.insert("*".to_string(), func(mul));
    repl_env.insert("/".to_string(), func(div));

    loop {
        let line = readline::mal_readline("user> ");
        match line { None => break, _ => () }
        match rep(&line.unwrap(), &repl_env) {
            Ok(str)  => println!("{}", str),
            Err(ErrMalVal(_)) => (),  // Blank line
            Err(ErrString(s)) => println!("Error: {}", s),
        }
    }
}
