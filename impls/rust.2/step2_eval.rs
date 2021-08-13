extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

mod reader;
mod types;
mod printer;
mod env;

fn read(input: String) -> Result<Option<crate::types::MalValue>, crate::types::MalError> {
    return reader::Reader::read_str(input.to_string());
}

fn to_int(value: types::MalValue) -> Result<i32, types::MalError> {
    match value {
        types::MalValue::MalInteger(int) => {
            return Ok(int);
        }
        _ => {
            return Err(types::MalError::EvalError(String::from("value is not an integer")));
        }
    }
}

fn add(args: &[types::MalValue]) -> types::MalValue {
    assert_eq!(2, args.len());
    let args1 = to_int(args[0].clone()).unwrap();
    let args2 = to_int(args[1].clone()).unwrap();

    return types::MalValue::MalInteger(args1 + args2);
}

fn subtract(args: &[types::MalValue]) -> types::MalValue {
    assert_eq!(2, args.len());
    let args1 = to_int(args[0].clone()).unwrap();
    let args2 = to_int(args[1].clone()).unwrap();

    return types::MalValue::MalInteger(args1 - args2);
}

fn multiply(args: &[types::MalValue]) -> types::MalValue {
    assert_eq!(2, args.len());
    let args1 = to_int(args[0].clone()).unwrap();
    let args2 = to_int(args[1].clone()).unwrap();

    return types::MalValue::MalInteger(args1 * args2);
}

fn divide(args: &[types::MalValue]) -> types::MalValue {
    assert_eq!(2, args.len());
    let args1 = to_int(args[0].clone()).unwrap();
    let args2 = to_int(args[1].clone()).unwrap();

    return types::MalValue::MalInteger(args1 / args2);
}

fn eval(ast: crate::types::MalValue, env: &env::Environment) -> Result<crate::types::MalValue, types::MalError>  {
    match ast.clone() {
        types::MalValue::MalList(list) => {
            if list.len() > 0 {
                let evaluated_result = eval_ast(types::MalValue::MalList(list), env)?;
                match evaluated_result {
                    types::MalValue::MalList(list) => {
                        let func = list.first().unwrap();
                        let args = &list[1..list.len()];

                        if let types::MalValue::MalFunction(f) = func {
                            if f == "+" {
                                return Ok(add(args));
                            } else if f == "-" {
                                return Ok(subtract(args));
                            } else if f == "*" {
                                return Ok(multiply(args));
                            } else if f == "/" {
                                return Ok(divide(args));
                            }
                            todo!("Handle other operators!");
                        }
                    },
                    _ => {
                        todo!("Handle eval_ast returning something that is not a list!");
                    }
                }
                // Call
            }
            return Ok(ast);
        },
        _ => {
            return eval_ast(ast, env);
        }
    }
}

fn eval_ast(ast: crate::types::MalValue, env: &env::Environment) -> Result<crate::types::MalValue, types::MalError> {
    match ast {
        types::MalValue::MalSymbol(symbol) => {
            if let Some(func) = env.lookup_symbol(String::from(symbol)) {
                return Ok(func);
            }
            return Err(types::MalError::EvalError(format!("Symbol {} not defined", "")));
        },
        types::MalValue::MalList(list) => {
            let mut result = Vec::<types::MalValue>::new();

            for token in list {
                result.push(eval(token, env)?);
            }

            return Ok(types::MalValue::MalList(result));
        },
        _ => {
            return Ok(ast);
        }
    }
}

fn print(input: crate::types::MalValue) -> String {
    return crate::printer::pr_str(input, true);
}

fn rep(input: String, env: &env::Environment) -> String {
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

    let mut env = env::Environment::new();
    env.add_symbol(String::from("+"), types::MalValue::MalFunction(String::from("+")));
    env.add_symbol(String::from("-"), types::MalValue::MalFunction(String::from("-")));
    env.add_symbol(String::from("/"), types::MalValue::MalFunction(String::from("/")));
    env.add_symbol(String::from("*"), types::MalValue::MalFunction(String::from("*")));

    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let result = rep(line.trim_end().to_string(), &env);
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