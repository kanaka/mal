use super::{MalValue, MalResult, MalError};
use super::printer::{pr_str};
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

fn add(args: Vec<MalValue>) -> MalValue {
    assert_eq!(2, args.len());
    let args1 = to_int(args[0].clone()).unwrap();
    let args2 = to_int(args[1].clone()).unwrap();

    return MalValue::MalInteger(args1 + args2);
}

fn subtract(args: Vec<MalValue>) -> MalValue {
    assert_eq!(2, args.len());
    let args1 = to_int(args[0].clone()).unwrap();
    let args2 = to_int(args[1].clone()).unwrap();

    return MalValue::MalInteger(args1 - args2);
}

fn multiply(args: Vec<MalValue>) -> MalValue {
    assert_eq!(2, args.len());
    let args1 = to_int(args[0].clone()).unwrap();
    let args2 = to_int(args[1].clone()).unwrap();

    return MalValue::MalInteger(args1 * args2);
}

fn divide(args: Vec<MalValue>) -> MalValue {
    assert_eq!(2, args.len());
    let args1 = to_int(args[0].clone()).unwrap();
    let args2 = to_int(args[1].clone()).unwrap();

    return MalValue::MalInteger(args1 / args2);
}

fn is_list(list:MalValue) -> MalValue {
    match list {
        MalValue::MalList(_) => MalValue::MalTrue,
        _ => MalValue::MalFalse
    }
}

fn print(args:Vec<MalValue>) -> MalValue {
    if args.is_empty() {
        MalValue::MalNil
    } else {
        MalValue::MalString(pr_str(args.first().unwrap().clone(), true))
    }
}

fn is_empty(args:Vec<MalValue>) -> MalValue {
    
    MalValue::MalFalse
}

pub fn ns() -> Vec<(&'static str, MalValue)> {
    vec![
        ("+", MalValue::MalFunction(|args| add(args), Rc::new(MalValue::MalNil)) ),
        ("-", MalValue::MalFunction(|args| subtract(args), Rc::new(MalValue::MalNil)) ),
        ("/", MalValue::MalFunction(|args| divide(args), Rc::new(MalValue::MalNil)) ),
        ("*", MalValue::MalFunction(|args| multiply(args), Rc::new(MalValue::MalNil)) ),
        ("prn", MalValue::MalFunction(|args| print(args), Rc::new(MalValue::MalNil))),
        ("list", MalValue::MalFunction(|args| MalValue::MalList(args), Rc::new(MalValue::MalNil))),
        ("list?", MalValue::MalFunction(|args| is_list(args.first().unwrap().clone()), Rc::new(MalValue::MalNil)))
    ]
}