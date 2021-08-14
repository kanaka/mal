use super::types::{MalValue, MalResult, MalError, bool, func};
use super::types::MalValue::{MalInteger, MalBool};
use super::printer::{pr_str};
use std::convert::TryInto;
use std::rc::Rc;

fn to_int(value: MalValue) -> Result<i32, MalError> {
    match value {
        MalValue::MalInteger(int) => {
            return Ok(int);
        }
        _ => {
            return Err(MalError::EvalError(String::from("value is not an integer")));
        }
    }
}

macro_rules! fn_ints {
    ($result:ident, $func:expr) => {{
        |args:Vec<MalValue>| match (args[0].clone(), args[1].clone()) {
            (MalInteger(a), MalInteger(b)) => Ok($result($func(a, b))),
            (_, _) => Err(MalError::EvalError("expecting (MalInteger, MalInteger) args!".to_string()))
        }
    }};
}

fn add(args: Vec<MalValue>) -> MalResult {
    assert_eq!(2, args.len());
    let args1 = to_int(args[0].clone()).unwrap();
    let args2 = to_int(args[1].clone()).unwrap();

    return Ok(MalValue::MalInteger(args1 + args2));
}

fn subtract(args: Vec<MalValue>) -> MalResult {
    assert_eq!(2, args.len());
    let args1 = to_int(args[0].clone()).unwrap();
    let args2 = to_int(args[1].clone()).unwrap();

    return Ok(MalValue::MalInteger(args1 - args2));
}

fn multiply(args: Vec<MalValue>) -> MalResult {
    assert_eq!(2, args.len());
    let args1 = to_int(args[0].clone()).unwrap();
    let args2 = to_int(args[1].clone()).unwrap();

    return Ok(MalValue::MalInteger(args1 * args2));
}

fn divide(args: Vec<MalValue>) -> MalResult {
    assert_eq!(2, args.len());
    let args1 = to_int(args[0].clone()).unwrap();
    let args2 = to_int(args[1].clone()).unwrap();

    return Ok(MalValue::MalInteger(args1 / args2));
}

fn is_list(list:MalValue) -> MalResult {
    match list {
        MalValue::MalList(_) => Ok(bool(true)),
        _ => Ok(bool(false))
    }
}

fn print(args:Vec<MalValue>) -> MalResult {
    if args.is_empty() {
        Ok(MalValue::MalNil)
    } else {
        Ok(MalValue::MalString(pr_str(args.first().unwrap().clone(), true)))
    }
}

fn is_empty(arg:MalValue) -> MalResult {
    match arg {
        MalValue::MalList(list) => Ok(bool(list.is_empty())),
        _ => Ok(MalValue::MalNil)
    }
}

fn count(arg:MalValue) -> MalResult {
    match arg.as_vec() {
        Some(vec) => Ok(MalValue::MalInteger(vec.len().try_into().unwrap())),
        _ => Ok(MalValue::MalNil)
    }
}



pub fn ns() -> Vec<(&'static str, MalValue)> {
    vec![
        ("+", MalValue::MalFunction(|args| add(args), Rc::new(MalValue::MalNil)) ),
        ("-", MalValue::MalFunction(|args| subtract(args), Rc::new(MalValue::MalNil)) ),
        ("/", MalValue::MalFunction(|args| divide(args), Rc::new(MalValue::MalNil)) ),
        ("*", MalValue::MalFunction(|args| multiply(args), Rc::new(MalValue::MalNil)) ),
        ("prn", MalValue::MalFunction(|args| print(args), Rc::new(MalValue::MalNil))),
        ("list", MalValue::MalFunction(|args| Ok(MalValue::MalList(args)), Rc::new(MalValue::MalNil))),
        ("list?", MalValue::MalFunction(|args| is_list(args.first().unwrap().clone()), Rc::new(MalValue::MalNil))),
        ("empty?", MalValue::MalFunction(|args| is_empty(args.first().unwrap().clone()), Rc::new(MalValue::MalNil))),
        ("count", MalValue::MalFunction(|args| count(args.first().unwrap().clone()), Rc::new(MalValue::MalNil))),
        ("=", MalValue::MalFunction(|args| Ok(bool(args[0].clone() == args[1].clone())), Rc::new(MalValue::MalNil))),
        ("<", func(fn_ints!(MalBool, |a,b| {a < b}))),
        ("<=", func(fn_ints!(MalBool, |a,b| {a <= b}))),
        (">", func(fn_ints!(MalBool, |a,b| {a > b}))),
        (">=", func(fn_ints!(MalBool, |a,b| {a >= b}))),
    ]
}