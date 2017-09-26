use types::MalVal;
use regex::Captures;

pub fn escape_str(s: &str) -> String {
    let mut escaped = String::new();
    escaped.push('"');
    for c in s.chars() {
        let _ = match c {
          '"' => escaped.push_str("\\\""),
          '\\' => escaped.push_str("\\\\"),
          '\x08' => escaped.push_str("\\b"),
          '\x0c' => escaped.push_str("\\f"),
          '\n' => escaped.push_str("\\n"),
          '\r' => escaped.push_str("\\r"),
          '\t' => escaped.push_str("\\t"),
          _ => escaped.push(c),
        };
    };

    escaped.push('"');

    escaped
}

pub fn unescape_str(s: &str) -> String {
    let re = regex!(r#"\\(.)"#);
    re.replace_all(&s, |caps: &Captures| {
        format!("{}", if &caps[1] == "n" { "\n" } else { &caps[1] })
    })
}

pub fn pr_list(lst: &Vec<MalVal>, pr: bool,
               start: &str , end: &str, join: &str) -> String {
    let mut first = true;
    let mut res = String::new();
    res.push_str(start);
    for mv in lst.iter() {
        if first {
            first = false;
        } else {
            res.push_str(join);
        }
        res.push_str(&mv.pr_str(pr));
    }
    res.push_str(end);
    res
}
