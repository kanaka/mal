// support precompiled regexes in reader.rs
#![feature(phase)]
#[phase(plugin)]
extern crate regex_macros;
extern crate regex;

use std::collections::HashMap;

use types::{MalVal,MalRet,Int,Sym,List,Func,
            _nil,_int,list,func};
mod readline;
mod types;
mod env;
mod reader;
mod printer;

// read
fn read(str: String) -> MalRet {
    reader::read_str(str)
}

// eval
fn eval_ast(ast: MalVal, env: &HashMap<String,MalVal>) -> MalRet {
    match *ast {
        Sym(ref sym) => {
            match env.find_copy(sym) {
                Some(mv) => Ok(mv),
                None     => Ok(_nil()),
            }
        },
        List(ref a) => {
            let mut ast_vec : Vec<MalVal> = vec![];
            for mv in a.iter() {
                let mv2 = mv.clone();
                match eval(mv2, env) {
                    Ok(mv) => { ast_vec.push(mv); },
                    Err(e) => { return Err(e); },
                }
            }
            Ok(list(ast_vec))
        },
        _ => {
            Ok(ast.clone())
        }
    }
}

fn eval(ast: MalVal, env: &HashMap<String,MalVal>) -> MalRet {
    let ast2 = ast.clone();
    match *ast2 {
        List(_) => (),  // continue
        _ => return eval_ast(ast2, env),
    }

    // apply list
    match eval_ast(ast, env) {
        Err(e) => Err(e),
        Ok(el) => {
            match *el {
                List(ref args) => {
                    // TODO: make this work
                    //match args.as_slice() {
                    //    [&Func(f), rest..] => {
                    //        (*f)(rest.to_vec())
                    //    },
                    //    _ => Err("attempt to call non-function".to_string()),
                    //}
                    let args2 = args.clone();
                    match *args2[0] {
                        Func(f) => f(args.slice(1,args.len()).to_vec()),
                        _ => Err("attempt to call non-function".to_string()),
                    }
                }
                _ => Err("Invalid apply".to_string()),
            }
        }
    }
}

// print
fn print(exp: MalVal) -> String {
    exp.pr_str(true)
}

fn rep(str: String, env: &HashMap<String,MalVal>) -> Result<String,String> {
    match read(str) {
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

fn int_op(f: |i:int,j:int|-> int, a:Vec<MalVal>) -> MalRet {
    match *a[0] {
        Int(a0) => match *a[1] {
            Int(a1) => Ok(_int(f(a0,a1))),
            _ => Err("second arg must be an int".to_string()),
        },
        _ => Err("first arg must be an int".to_string()),
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
        match rep(line.unwrap(), &repl_env) {
            Ok(str)  => println!("{}", str),
            Err(str) => println!("Error: {}", str),
        }
    }
}
