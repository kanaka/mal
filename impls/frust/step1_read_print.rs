const HISTORY: &str = "history.txt";

mod printer;
mod reader;
mod types;

fn main() {
    let rl_config = rustyline::Config::builder()
        .auto_add_history(true)
        .completion_type(rustyline::CompletionType::Circular)
        .edit_mode(rustyline::EditMode::Vi)
        .color_mode(rustyline::ColorMode::Enabled)
        .build();
    let mut rl = rustyline::Editor::<()>::with_config(rl_config);
    let _ = rl.load_history(HISTORY);
    for readline in rl.iter("user> ") {
        match readline {
            Ok(s) => {
                match rep(s) {
                    Ok(s) => println!("{}", s),
                    Err(e) => eprintln!("{}", e),
                };
            }
            Err(e) => eprintln!("{}", e),
        }
    }
    rl.save_history(HISTORY).expect("Failed to save history");
}

fn rep(s: String) -> Result<String> {
    let r = read(s)?;
    let e = eval(r)?;
    Ok(print(e))
}

fn read(s: String) -> MalResult {
    reader::read_str(s)
}

fn eval(t: types::MalType) -> MalResult {
    Ok(t)
}

fn print(t: types::MalType) -> String {
    printer::pr_str(&t, true)
}

#[derive(Debug)]
enum MalError {
    MismatchedParen,
    OddNumParamsInMap,
    NonStringKey,
    StringMismatchedDoubleQuote,
    StringEscape,
}

impl std::fmt::Display for MalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MismatchedParen => write!(f, "unbalanced parenthesis"),
            Self::OddNumParamsInMap => write!(f, "a map needs an even number of parameters"),
            Self::NonStringKey => write!(f, "maps can only have strings or keywords as keys"),
            Self::StringEscape => write!(f, "bad string escape sequence"),
            Self::StringMismatchedDoubleQuote => write!(f, "unbalanced double quotes in string"),
        }
    }
}

type Result<T> = std::result::Result<T, MalError>;
type MalResult = Result<types::MalType>;
