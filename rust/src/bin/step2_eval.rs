extern crate mal;

use std::collections::HashMap;

use mal::types::{MalVal, MalRet, MalError, err_str, err_string};
use mal::types::{list, vector, hash_map, _int, func};
use mal::types::MalError::{ErrString, ErrMalVal};
use mal::types::MalType::{Sym, List, Vector, Hash_Map, Int};
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
                //None     => Ok(_nil()),
                None     => err_string(format!("'{}' not found", sym)),
            }
        },
        List(ref a,_) | Vector(ref a,_) => {
            let mut ast_vec : Vec<MalVal> = vec![];
            for mv in a.iter() {
                let mv2 = mv.clone();
                ast_vec.push(try!(eval(mv2, env)));
            }
            Ok(match *ast { List(_,_) => list(ast_vec),
                            _         => vector(ast_vec) })
        }
        Hash_Map(ref hm,_) => {
            let mut new_hm: HashMap<String,MalVal> = HashMap::new();
            for (key, value) in hm.iter() {
                new_hm.insert(key.to_string(),
                              try!(eval(value.clone(), env)));
            }
            Ok(hash_map(new_hm))
        }
        _ => Ok(ast.clone()),
    }
}

fn eval(ast: MalVal, env: &HashMap<String,MalVal>) -> MalRet {
    //println!("eval: {}", ast);
    match *ast {
        List(_,_) => (),  // continue
        _ => return eval_ast(ast, env),
    }

    // apply list
    match *try!(eval_ast(ast, env)) {
        List(ref args,_) => {
            match args.len() {
                0 =>
                    Ok(list(vec![])),
                _ =>  {
                    let ref f = args.clone()[0];
                    f.apply(args[1..].to_vec())
                }
            }
        },
        _ => return err_str("Expected list"),
    }
}

// print
fn print(exp: MalVal) -> String {
    exp.pr_str(true)
}

fn rep(str: &str, env: &HashMap<String,MalVal>) -> Result<String,MalError> {
    let ast = try!(read(str.to_string()));
    //println!("read: {}", ast);
    let exp = try!(eval(ast, env));
    Ok(print(exp))
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
