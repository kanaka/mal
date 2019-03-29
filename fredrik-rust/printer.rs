use super::types::MalType;

pub fn pr_str(val: &MalType) -> String {
    match val {
        MalType::List(lst) => {
            let inner: Vec<String> = lst.iter().map(pr_str).collect();
            format!("({})", inner.join(" "))
        }
        MalType::Vector(lst) => {
            let inner: Vec<String> = lst.iter().map(pr_str).collect();
            format!("[{}]", inner.join(" "))
        }
        MalType::HashMap(lst) => {
            let inner: Vec<String> = lst.iter().map(pr_str).collect();
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
        MalType::Quote(s) => format!("(quote {})", pr_str(s)),
        MalType::QuasiQuote(s) => format!("(quasiquote {})", pr_str(s)),
        MalType::UnQuote(s) => format!("(unquote {})", pr_str(s)),
        MalType::SpliceUnQuote(s) => format!("(splice-unquote {})", pr_str(s)),
        MalType::WithMeta(a, b) => format!("(with-meta {} {})", pr_str(a), pr_str(b)),
        MalType::Deref(a) => format!("(deref {})", a),
        MalType::Nil => "nil".to_owned(),
        MalType::String(s) => s.to_owned(),
        MalType::BIF(_) => "<fn>".to_owned(),
    }
}
