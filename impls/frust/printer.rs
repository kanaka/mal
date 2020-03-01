use super::types::MalType;

pub fn pr_str(t: &MalType, print_readably: bool) -> String {
    match t {
        // MalType::Keyword(s) => s.to_owned(),
        MalType::Deref(t) => format!("(deref {})", pr_str(t, print_readably)),
        MalType::Nil => "nil".to_owned(),
        MalType::Number(n) => format!("{}", n),
        MalType::String(s) if s.starts_with(super::types::KEYWORD_ESC) => {
            let rest: String = s.chars().skip(1).collect();
            format!(":{}", rest)
        }
        MalType::String(s) => {
            if print_readably {
                let mut res = String::new();
                res.push('\"');

                for c in s.chars() {
                    if c == '\\' {
                        res.push('\\');
                        res.push('\\');
                    } else if c == '\"' {
                        res.push('\\');
                        res.push('\"');
                    } else if c == '\n' {
                        res.push('\\');
                        res.push('n');
                    } else {
                        res.push(c)
                    }
                }
                res.push('\"');

                res
            } else {
                s.clone()
            }
        }
        MalType::Quote(t) => format!("(quote {})", pr_str(t, print_readably)),
        MalType::QuasiQuote(t) => format!("(quasiquote {})", pr_str(t, print_readably)),
        MalType::Symbol(s) => s.clone(),
        MalType::List(l, meta) => {
            let res = format!(
                "({})",
                l.iter()
                    .map(|v| pr_str(&v, print_readably))
                    .collect::<Vec<String>>()
                    .join(" ")
            );
            with_meta(res, meta, print_readably)
        }
        MalType::Vector(l, meta) => {
            let res = format!(
                "[{}]",
                l.iter()
                    .map(|v| pr_str(&v, print_readably))
                    .collect::<Vec<String>>()
                    .join(" ")
            );
            with_meta(res, meta, print_readably)
        }
        MalType::HashMap(l, meta) => {
            let res = format!(
                "{{{}}}",
                l.iter()
                    .map(|(k, v)| format!(
                        "{} {}",
                        pr_str(&MalType::String(k.clone()), print_readably),
                        pr_str(&v, print_readably)
                    ))
                    .collect::<Vec<String>>()
                    .join(" ")
            );
            with_meta(res, meta, print_readably)
        }
        MalType::Unquote(t) => format!("(unquote {})", pr_str(t, print_readably)),
        MalType::SpliceUnquote(t) => format!("(splice-unquote {})", pr_str(t, print_readably)),
    }
}

fn with_meta(s: String, meta: &Option<Box<MalType>>, print_readably: bool) -> String {
    match meta {
        Some(meta) => format!("(with-meta {} {})", s, pr_str(&meta, print_readably)),
        None => s,
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn pr_str() {
        use crate::types::MalType;
        let list = MalType::List(vec![]);

        assert_eq!(super::pr_str(&list), "()");

        let list = MalType::List(vec![Box::new(MalType::Nil)]);

        assert_eq!(super::pr_str(&list), "(nil)");

        let list = MalType::List(vec![
            Box::new(MalType::Number(12.0)),
            Box::new(MalType::Number(22.0)),
        ]);

        assert_eq!(super::pr_str(&list), "(12 22)");
    }
}
