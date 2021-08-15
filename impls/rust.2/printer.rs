use super::types::MalValue;
use super::types::MalValue::{MalBool, MalInteger, MalNil};

fn escape_chars(input: &str) -> String {
    let s = input.strip_prefix('"').unwrap().strip_suffix('"').unwrap();

    s.chars()
        .map(|c| match c {
            '"' => "\\\"".to_string(),
            '\n' => "\\n".to_string(),
            '\\' => "\\\\".to_string(),
            _ => c.to_string(),
        })
        .collect::<Vec<String>>()
        .join("")
}

macro_rules! s {
    ($val:expr) => {
        String::from($val)
    };
}

fn pr_hash(keys: Vec<MalValue>, values: Vec<MalValue>, print_readably: bool) -> String {
    let vals: Vec<String> = keys
        .iter()
        .zip(values.iter())
        .map(|(k, v)| -> String {
            format!(
                "{} {}",
                pr_str(k.clone(), print_readably),
                pr_str(v.clone(), print_readably)
            )
        })
        .collect();

    format!("{{{}}}", vals.join(" "))
}

fn pr_vec(val: Vec<MalValue>, print_readably: bool) -> String {
    let strs: Vec<String> = val
        .iter()
        .map(|x| pr_str(x.clone(), print_readably))
        .collect();
    strs.join(" ")
}

pub fn pr_str(val: MalValue, print_readably: bool) -> String {
    return match val {
        MalInteger(i) => i.to_string(),
        MalBool(b) => b.to_string(),
        MalNil => s!("nil"),
        MalValue::MalKeyword(kw) => format!(":{}", kw),
        MalValue::MalSymbol(s) => s,
        MalValue::MalFunc { .. } => s!("#<function>"),
        MalValue::MalFunction(_, _) => s!("#<function>"),
        MalValue::MalList(l) => format!("({})", pr_vec(l, print_readably)),
        MalValue::MalVector(v) => format!("[{}]", pr_vec(v, print_readably)),
        MalValue::MalString(s) => {
            if print_readably {
                format!("\"{}\"", escape_chars(&s))
            } else {
                s.clone()
                    .strip_prefix('"')
                    .unwrap()
                    .strip_suffix('"')
                    .unwrap()
                    .to_string()
            }
        }
        MalValue::MalHashmap(keys, values) => pr_hash(keys, values, print_readably),
        _ => "??".to_string(),
    };
}
