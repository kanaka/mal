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
        MalType::Deref(a) => format!("(deref {})", a),
        MalType::Nil => "nil".to_owned(),
        MalType::String(s) => print_string(s, print_readably),
        MalType::Fn(_) => "#".to_owned(),
    }
}

fn print_string(s: &str, print_readably: bool) -> String {
    let res = if print_readably {
        s.to_string()
            .replace(r#"\""#, "\"")
            .replace(r#"\n"#, "\n")
            .replace(r#"\\"#, "\\")
    } else {
        s.to_string()
    };

    format!(r#""{}""#, res)
}
