use super::printer::pr_str;
use super::types::MalValue::{MalBool, MalInteger};
use super::types::{bool, func, MalError, MalResult, MalValue};
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
        |args: Vec<MalValue>| match (args[0].clone(), args[1].clone()) {
            (MalInteger(a), MalInteger(b)) => Ok($result($func(a, b))),
            (_, _) => Err(MalError::EvalError(
                "expecting (MalInteger, MalInteger) args!".to_string(),
            )),
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

fn is_list(list: MalValue) -> MalResult {
    match list {
        MalValue::MalList(_) => Ok(bool(true)),
        _ => Ok(bool(false)),
    }
}

fn print(args: Vec<MalValue>) -> MalResult {
    if args.is_empty() {
        Ok(MalValue::MalNil)
    } else {
        let out = pr_str(args.first().unwrap().clone(), true);
        print!("{}", out);
        Ok(MalValue::MalString(out))
    }
}

fn is_empty(arg: MalValue) -> MalResult {
    match arg.as_vec() {
        Some(vec) => Ok(bool(vec.is_empty())),
        _ => Ok(MalValue::MalNil),
    }
}

fn count(arg: MalValue) -> MalResult {
    match arg.as_vec() {
        Some(vec) => Ok(MalValue::MalInteger(vec.len().try_into().unwrap())),
        _ => Ok(MalValue::MalInteger(0)),
    }
}

fn print_vals(
    arg: Vec<MalValue>,
    print_readably: bool,
    seperator: String,
    output: bool,
) -> MalResult {
    let mut r = arg.iter().fold(String::new(), |mut res, v| {
        res += &pr_str(v.clone(), print_readably).to_string();
        res += &seperator.to_string();
        res
    });

    r = r.trim_end().to_string();

    if output {
        println!("{}", r);
        return Ok(MalValue::MalNil);
    }

    Ok(MalValue::MalString(format!("\"{}\"", r)))
}

pub fn ns() -> Vec<(&'static str, MalValue)> {
    vec![
        (
            "+",
            MalValue::MalFunction(|args| add(args), Rc::new(MalValue::MalNil)),
        ),
        (
            "-",
            MalValue::MalFunction(|args| subtract(args), Rc::new(MalValue::MalNil)),
        ),
        (
            "/",
            MalValue::MalFunction(|args| divide(args), Rc::new(MalValue::MalNil)),
        ),
        (
            "*",
            MalValue::MalFunction(|args| multiply(args), Rc::new(MalValue::MalNil)),
        ),
        (
            "prn",
            MalValue::MalFunction(|args| print(args), Rc::new(MalValue::MalNil)),
        ),
        (
            "list",
            MalValue::MalFunction(
                |args| Ok(MalValue::MalList(args)),
                Rc::new(MalValue::MalNil),
            ),
        ),
        (
            "list?",
            MalValue::MalFunction(
                |args| is_list(args.first().unwrap().clone()),
                Rc::new(MalValue::MalNil),
            ),
        ),
        (
            "empty?",
            MalValue::MalFunction(
                |args| is_empty(args.first().unwrap().clone()),
                Rc::new(MalValue::MalNil),
            ),
        ),
        (
            "count",
            MalValue::MalFunction(
                |args| count(args.first().unwrap().clone()),
                Rc::new(MalValue::MalNil),
            ),
        ),
        (
            "=",
            MalValue::MalFunction(
                |args| Ok(bool(args[0].clone() == args[1].clone())),
                Rc::new(MalValue::MalNil),
            ),
        ),
        ("<", func(fn_ints!(MalBool, |a, b| { a < b }))),
        ("<=", func(fn_ints!(MalBool, |a, b| { a <= b }))),
        (">", func(fn_ints!(MalBool, |a, b| { a > b }))),
        (">=", func(fn_ints!(MalBool, |a, b| { a >= b }))),
        (
            "pr-str",
            func(|v| print_vals(v, true, " ".to_string(), false)),
        ),
        ("str", func(|v| print_vals(v, false, "".to_string(), false))),
        ("prn", func(|v| print_vals(v, true, " ".to_string(), true))),
        (
            "println",
            func(|v| print_vals(v, false, " ".to_string(), true)),
        ),
    ]
}
