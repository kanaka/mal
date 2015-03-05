#![feature(exit_status)]

extern crate mal;

use std::collections::HashMap;
use std::env as stdenv;

use mal::types::{MalVal, MalRet, MalError, err_str};
use mal::types::{symbol, _nil, string, list, vector, hash_map, malfunc, malfuncd};
use mal::types::MalError::{ErrString, ErrMalVal};
use mal::types::MalType::{Nil, False, Sym, List, Vector, Hash_Map, Func, MalFunc};
use mal::{readline, reader, core};
use mal::env::{env_set, env_get, env_new, env_bind, env_find, env_root, Env};


// read
fn read(str: String) -> MalRet {
    reader::read_str(str)
}

// eval
fn is_pair(x: MalVal) -> bool {
    match *x {
        List(ref lst,_) | Vector(ref lst,_) => lst.len() > 0,
        _ => false,
    }
}

fn quasiquote(ast: MalVal) -> MalVal {
    if !is_pair(ast.clone()) {
        return list(vec![symbol("quote"), ast])
    }

    match *ast.clone() {
        List(ref args,_) | Vector(ref args,_) => {
            let ref a0 = args[0];
            match **a0 {
                Sym(ref s) if *s == "unquote" => return args[1].clone(),
                _ => (),
            }
            if is_pair(a0.clone()) {
                match **a0 {
                    List(ref a0args,_) | Vector(ref a0args,_) => {
                        match *a0args[0] {
                            Sym(ref s) if *s == "splice-unquote" => {
                                return list(vec![symbol("concat"),
                                                 a0args[1].clone(),
                                                 quasiquote(list(args[1..].to_vec()))])
                            },
                            _ => (),
                        }
                    },
                    _ => (),
                }
            }
            let rest = list(args[1..].to_vec());
            return list(vec![symbol("cons"),
                             quasiquote(a0.clone()),
                             quasiquote(rest)])
        },
        _ => _nil(), // should never reach
    }
}

fn is_macro_call(ast: MalVal, env: Env) -> bool {
    let lst = match *ast {
        List(ref lst,_) => &lst[0],
        _ => return false
    };
    match **lst {
        Sym(_) => {},
        _ => return false
    }
    if env_find(&env, lst).is_none() {
        return false
    }
    let f = match env_get(&env, lst) {
        Ok(f) => f,
        _ => return false
    };
    match *f {
        MalFunc(ref mfd,_) => mfd.is_macro,
        _ => false,
    }
}

fn macroexpand(mut ast: MalVal, env: Env) -> MalRet {
    while is_macro_call(ast.clone(), env.clone()) {
        let ast2 = ast.clone();
        let args = match *ast2 {
            List(ref args,_) => args,
            _ => break,
        };
        let ref a0 = args[0];
        let mf = match **a0 {
            Sym(_) => try!(env_get(&env, &a0)),
            _ => break,
        };
        match *mf {
            MalFunc(_,_) => ast = try!(mf.apply(args[1..].to_vec())),
            _ => break,
        }
    }
    Ok(ast)
}

fn eval_ast(ast: MalVal, env: Env) -> MalRet {
    match *ast {
        Sym(_) => env_get(&env, &ast),
        List(ref a,_) | Vector(ref a,_) => {
            let mut ast_vec : Vec<MalVal> = vec![];
            for mv in a.iter() {
                let mv2 = mv.clone();
                ast_vec.push(try!(eval(mv2, env.clone())));
            }
            Ok(match *ast { List(_,_) => list(ast_vec),
                            _         => vector(ast_vec) })
        }
        Hash_Map(ref hm,_) => {
            let mut new_hm: HashMap<String,MalVal> = HashMap::new();
            for (key, value) in hm.iter() {
                new_hm.insert(key.to_string(),
                              try!(eval(value.clone(), env.clone())));
            }
            Ok(hash_map(new_hm))
        }
        _ => Ok(ast.clone()),
    }
}

fn eval(mut ast: MalVal, mut env: Env) -> MalRet {
    'tco: loop {

    //println!("eval: {}, {}", ast, env.borrow());
    //println!("eval: {}", ast);
    match *ast {
        List(_,_) => (),  // continue
        _ => return eval_ast(ast, env),
    }

    // apply list
    ast = try!(macroexpand(ast, env.clone()));
    match *ast {
        List(_,_) => (),  // continue
        _ => return Ok(ast),
    }

    let tmp = ast;
    let (args, a0sym) = match *tmp {
        List(ref args,_) => {
            if args.len() == 0 {
                return Ok(tmp.clone());
            }
            let ref a0 = *args[0];
            match *a0 {
                Sym(ref a0sym) => (args, &a0sym[..]),
                _ => (args, "__<fn*>__"),
            }
        },
        _ => return err_str("Expected list"),
    };

    match a0sym {
        "def!" => {
            let a1 = (*args)[1].clone();
            let a2 = (*args)[2].clone();
            let r = try!(eval(a2, env.clone()));
            match *a1 {
                Sym(_) => {
                    env_set(&env.clone(), a1, r.clone());
                    return Ok(r);
                },
                _ => return err_str("def! of non-symbol"),
            }
        },
        "let*" => {
            let let_env = env_new(Some(env.clone()));
            let a1 = (*args)[1].clone();
            let a2 = (*args)[2].clone();
            match *a1 {
                List(ref binds,_) | Vector(ref binds,_) => {
                    let mut it = binds.iter();
                    while it.len() >= 2 {
                        let b = it.next().unwrap();
                        let exp = it.next().unwrap();
                        match **b {
                            Sym(_) => {
                                let r = try!(eval(exp.clone(), let_env.clone()));
                                env_set(&let_env, b.clone(), r);
                            },
                            _ => return err_str("let* with non-symbol binding"),
                        }
                    }
                },
                _ => return err_str("let* with non-list bindings"),
            }
            ast = a2;
            env = let_env.clone();
            continue 'tco;
        },
        "quote" => return Ok((*args)[1].clone()),
        "quasiquote" => {
            let a1 = (*args)[1].clone();
            ast = quasiquote(a1);
            continue 'tco;
        },
        "defmacro!" => {
            let a1 = (*args)[1].clone();
            let a2 = (*args)[2].clone();
            let r = try!(eval(a2, env.clone()));
            match *r {
                MalFunc(ref mfd,_) => {
                    match *a1 {
                        Sym(_) => {
                            let mut new_mfd = mfd.clone();
                            new_mfd.is_macro = true;
                            let mf = malfuncd(new_mfd,_nil());
                            env_set(&env.clone(), a1.clone(), mf.clone());
                            return Ok(mf);
                        },
                        _ => return err_str("def! of non-symbol"),
                    }
                },
                _ => return err_str("def! of non-symbol"),
            }
        },
        "macroexpand" => {
            let a1 = (*args)[1].clone();
            return macroexpand(a1, env.clone())
        },
        "try*" => {
            let a1 = (*args)[1].clone();
            match eval(a1, env.clone()) {
                Ok(res) => return Ok(res),
                Err(err) => {
                    if args.len() < 3 { return Err(err); }
                    let a2 = (*args)[2].clone();
                    let cat = match *a2 {
                        List(ref cat,_) => cat,
                        _ => return err_str("invalid catch* clause"),
                    };
                    if cat.len() != 3 {
                        return err_str("wrong arity to catch* clause");
                    }
                    let c1 = (*cat)[1].clone();
                    match *c1 {
                        Sym(_) => {},
                        _ => return err_str("invalid catch* binding"),
                    };
                    let exc = match err {
                        ErrMalVal(mv) => mv,
                        ErrString(s) => string(s),
                    };
                    let bind_env = env_new(Some(env.clone()));
                    env_set(&bind_env, c1.clone(), exc);
                    let c2 = (*cat)[2].clone();
                    return eval(c2, bind_env);
                },
            };
        }
        "do" => {
            let el = list(args[1..args.len()-1].to_vec());
            try!(eval_ast(el, env.clone()));
            ast = args[args.len() - 1].clone();
            continue 'tco;
        },
        "if" => {
            let a1 = (*args)[1].clone();
            let c = try!(eval(a1, env.clone()));
            match *c {
                False | Nil => {
                    if args.len() >= 4 {
                        ast = args[3].clone();
                        continue 'tco;
                    } else {
                        return Ok(_nil());
                    }
                },
                _ => {
                    ast = args[2].clone();
                    continue 'tco;
                },
            }
        },
        "fn*" => {
            let a1 = args[1].clone();
            let a2 = args[2].clone();
            return Ok(malfunc(eval, a2, env, a1, _nil()));
        },
        "eval" => {
            let a1 = (*args)[1].clone();
            ast = try!(eval(a1, env.clone()));
            env = env_root(&env);
            continue 'tco;
        },
        _ => { // function call
            let el = try!(eval_ast(tmp.clone(), env.clone()));
            let args = match *el {
                List(ref args,_) => args,
                _ => return err_str("Invalid apply"),
            };
            return match *args.clone()[0] {
                Func(f,_) => f(args[1..].to_vec()),
                MalFunc(ref mf,_) => {
                    let mfc = mf.clone();
                    let alst = list(args[1..].to_vec());
                    let new_env = env_new(Some(mfc.env.clone()));
                    match env_bind(&new_env, mfc.params, alst) {
                        Ok(_) => {
                            ast = mfc.exp;
                            env = new_env;
                            continue 'tco;
                        },
                        Err(e) => err_str(&e),
                    }
                },
                _ => err_str("attempt to call non-function"),
            }
        },
    }

    }
}

// print
fn print(exp: MalVal) -> String {
    exp.pr_str(true)
}

fn rep(str: &str, env: Env) -> Result<String,MalError> {
    let ast = try!(read(str.to_string()));
    //println!("read: {}", ast);
    let exp = try!(eval(ast, env));
    Ok(print(exp))
}

fn main() {
    // core.rs: defined using rust
    let repl_env = env_new(None);
    for (k, v) in core::ns().into_iter() {
        env_set(&repl_env, symbol(&k), v);
    }
    // see eval() for definition of "eval"
    env_set(&repl_env, symbol("*ARGV*"), list(vec![]));

    // core.mal: defined using the language itself
    let _ = rep("(def! not (fn* (a) (if a false true)))", repl_env.clone());
    let _ = rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))", repl_env.clone());
    let _ = rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))", repl_env.clone());
    let _ = rep("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))", repl_env.clone());

    // Invoked with command line arguments
    let args = stdenv::args();
    if args.len() > 1 {
        let mv_args = args.skip(2)
            .map(|a| string(a))
            .collect::<Vec<MalVal>>();
        env_set(&repl_env, symbol("*ARGV*"), list(mv_args));
        let lf = format!("(load-file \"{}\")",
                         stdenv::args().skip(1).next().unwrap());
        return match rep(&lf, repl_env.clone()) {
            Ok(_) => stdenv::set_exit_status(0),
            Err(str) => {
                println!("Error: {:?}", str);
                stdenv::set_exit_status(1);
            }
        };
    }

    // repl loop
    loop {
        let line = readline::mal_readline("user> ");
        match line { None => break, _ => () }
        match rep(&line.unwrap(), repl_env.clone()) {
            Ok(str)  => println!("{}", str),
            Err(ErrMalVal(_)) => (),  // Blank line
            Err(ErrString(s)) => println!("Error: {}", s),
        }
    }
}
