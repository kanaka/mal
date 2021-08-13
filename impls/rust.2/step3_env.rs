extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

mod reader;
mod types;
mod printer;
mod env;

use types::{MalValue, MalError, MalResult};
use std::rc::Rc;

fn read(input: String) -> Result<Option<MalValue>, MalError> {
    return reader::Reader::read_str(input.to_string());
}

fn to_int(value: MalValue) -> Result<i32, MalError> {
    match value {
        MalValue::MalInteger(int) => {
            return Ok(int);
        }
        _ => {
            return Err(types::MalError::EvalError(String::from("value is not an integer")));
        }
    }
}

fn add(args: Vec<types::MalValue>) -> MalValue {
    assert_eq!(2, args.len());
    let args1 = to_int(args[0].clone()).unwrap();
    let args2 = to_int(args[1].clone()).unwrap();

    return MalValue::MalInteger(args1 + args2);
}

fn subtract(args: Vec<types::MalValue>) -> MalValue {
    assert_eq!(2, args.len());
    let args1 = to_int(args[0].clone()).unwrap();
    let args2 = to_int(args[1].clone()).unwrap();

    return MalValue::MalInteger(args1 - args2);
}

fn multiply(args: Vec<types::MalValue>) -> MalValue {
    assert_eq!(2, args.len());
    let args1 = to_int(args[0].clone()).unwrap();
    let args2 = to_int(args[1].clone()).unwrap();

    return MalValue::MalInteger(args1 * args2);
}

fn divide(args: Vec<types::MalValue>) -> MalValue {
    assert_eq!(2, args.len());
    let args1 = to_int(args[0].clone()).unwrap();
    let args2 = to_int(args[1].clone()).unwrap();

    return MalValue::MalInteger(args1 / args2);
}

fn apply(ast: MalValue, env: Rc<env::Environment>) -> MalResult {
    match ast {
        MalValue::MalList(list) => {
            let evaluated_result = eval_ast(types::MalValue::MalList(list), env)?;
            match evaluated_result {
                MalValue::MalList(list) => {
                    let func = list.first().unwrap();
                    let args = list.clone().split_off(1);

                    if let MalValue::MalFunction(f) = func {
                        return Ok(f(args));
                    }
                    return Err(MalError::EvalError(String::from("apply called on non-list")));
                },
                _ => {
                    todo!("Handle eval_ast returning something that is not a list!");
                }
            }
        },
        _ => {
            return Err(MalError::EvalError(String::from("apply called on non-list")));
        }
    }
}

fn run_let(env: Rc<env::Environment>, bindings: MalValue, e: MalValue) -> MalResult {
    let new_env = Rc::new(env::Environment::new(Some(env.clone())));

    if let Some(binding_list) = bindings.as_vec() {
        assert!(binding_list.len() % 2 == 0);
        let mut key = binding_list.first().unwrap();

        for i in 0..binding_list.len() {
            if i % 2 == 0 {
                key = binding_list.get(i).unwrap();
            } else {
                new_env.set(
                    key.clone(),
                    eval(binding_list.get(i).unwrap().clone(), new_env.clone())?);
            }
        }

        return eval(e, new_env);
    } else {
        return Err(MalError::EvalError(String::from("Missing bindings!")))
    }
}

fn eval(ast:  MalValue, env: Rc<env::Environment>) -> MalResult  {
    match ast.clone() {
        MalValue::MalList(list) => {
            if list.len() > 0 {
                match list.first().unwrap() {
                    MalValue::MalSymbol(ref s) => {
                        if s == "def!" {
                            assert_eq!(3, list.len());
                            let key = list[1].clone();
                            let value = eval(list[2].clone(), env.clone())?;
                            env.set(key, value.clone())?;
                            return Ok(value);
                        } else if s == "let*" {
                            assert_eq!(3, list.len());
                            let bindings = list[1].clone();
                            let e = list[2].clone();
                            return run_let(env, bindings, e);
                        } else {
                            return apply(ast, env);
                        }
                    }
                    _ => {
                        return apply(ast, env);
                    }
                }
            }
            return Ok(ast);
        },
        _ => {
            return eval_ast(ast, env);
        }
    }
}

fn eval_ast(ast:  MalValue, env: Rc<env::Environment>) -> Result< MalValue, MalError> {
    match ast {
        MalValue::MalSymbol(_) => {
            if let Ok(func) = env.get(ast.clone()) {
                return Ok(func);
            }
            return Err(types::MalError::EvalError(format!("Symbol '{}' not found", ast.inspect(false))));
        },
        MalValue::MalList(list) => {
            let mut result = Vec::<types::MalValue>::new();

            for token in list {
                result.push(eval(token, env.clone())?);
            }

            return Ok(types::MalValue::MalList(result));
        },
        MalValue::MalVector(vector) => {
            let mut result = Vec::<types::MalValue>::new();

            for token in vector {
                result.push(eval(token, env.clone())?);
            }

            return Ok(types::MalValue::MalVector(result));
        },
        MalValue::MalHashmap(keys, values) => {
            let mut result = Vec::<types::MalValue>::new();

            for token in values {
                result.push(eval(token, env.clone())?);
            }

            return Ok(types::MalValue::MalHashmap(keys, result));
        }
        _ => {
            return Ok(ast);
        }
    }
}

fn print(input:  MalValue) -> String {
    return crate::printer::pr_str(input, true);
}

fn rep(input: String, env: Rc<env::Environment>) -> String {
    let ast = read(input);
    match ast {
        Err(e) => {
            return format!("Error! {:?}", e);
        }
        Ok(v) => {
            match v {
                None => return String::default(),
                Some(a) => {
                    match eval(a, env) {
                        Ok(result) => {
                            return print(result);
                        },
                        Err(e) => {
                            return format!("Error! {:?}", e);
                        }
                    }
                }
            }
        }
    }    
}


fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history(".mal-history").is_err() {
        println!("No previous history.");
    }

    let mut env = Rc::new(env::Environment::new(None));
    env.set(MalValue::MalSymbol(String::from("+")), MalValue::MalFunction(|args| add(args)));
    env.set(MalValue::MalSymbol(String::from("-")), MalValue::MalFunction(|args| subtract(args)));
    env.set(MalValue::MalSymbol(String::from("/")), MalValue::MalFunction(|args| divide(args)));
    env.set(MalValue::MalSymbol(String::from("*")), MalValue::MalFunction(|args| multiply(args)));

    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let result = rep(line.trim_end().to_string(), env.clone());
                print!("{}", result);
                println!();
            },
            Err(ReadlineError::Interrupted) => {
                break
            },
            Err(ReadlineError::Eof) => {
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
}