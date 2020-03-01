use super::types::MalType;

pub fn pr_str(val: &MalType, print_readably: bool) -> String {
    match val {
        MalType::List(lst) => {
            let inner: Vec<String> = lst.iter().map(|s| pr_str(s, print_readably)).collect();
            format!("({})", inner.join(" "))
        }
        MalType::Vector(lst) => {
            let inner: Vec<String> = lst.iter().map(|s| pr_str(s, print_readably)).collect();
            format!("[{}]", inner.join(" "))
        }
        MalType::HashMap(lst) => {
            let inner: Vec<String> = lst.iter().map(|s| pr_str(s, print_readably)).collect();
            format!("{{{}}}", inner.join(" "))
        }
        MalType::Integer(i) => format!("{}", i),
        MalType::Symbol(s) => s.to_owned(),
        MalType::Boolean(b) => {
            if *b {
                "true".to_owned()
            } else {
                "false".to_owned()
            }
        }
        MalType::Quote(s) => format!("(quote {})", pr_str(s, print_readably)),
        MalType::QuasiQuote(s) => format!("(quasiquote {})", pr_str(s, print_readably)),
        MalType::UnQuote(s) => format!("(unquote {})", pr_str(s, print_readably)),
        MalType::SpliceUnQuote(s) => format!("(splice-unquote {})", pr_str(s, print_readably)),
        MalType::WithMeta(a, b) => format!(
            "(with-meta {} {})",
            pr_str(a, print_readably),
            pr_str(b, print_readably)
        ),
        MalType::Keyword(k) => format!(":{}", k),
        MalType::Deref(a) => format!("(deref {})", a),
        MalType::Nil => "nil".to_owned(),
        MalType::String(s) => print_string(s, print_readably),
        MalType::Fn(_) => "#".to_owned(),
        MalType::MalFunc(_ast, params, _, fun) => format!(
            "fn ({}) {}",
            pr_str(&MalType::List(params.to_vec()), print_readably),
            pr_str(fun, print_readably)
        ),
    }
}

fn escape_str(s: &str) -> String {
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

fn print_string(s: &str, print_readably: bool) -> String {
    if print_readably {
        format!(r#""{}""#, escape_str(s))
    } else {
        // format!(r#"{}"#, s)
        s.to_string()
    }
}
