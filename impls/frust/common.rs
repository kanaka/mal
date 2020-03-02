#[derive(Debug)]
pub enum MalError {
    MismatchedParen,
    OddNumParamsInMap,
    NonStringKey,
    StringMismatchedDoubleQuote,
    StringEscape,
    NonNumericArguments,
    SymbolNotFound(String),
}

impl std::fmt::Display for MalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MismatchedParen => write!(f, "unbalanced parenthesis"),
            Self::OddNumParamsInMap => write!(f, "a map needs an even number of parameters"),
            Self::NonStringKey => write!(f, "maps can only have strings or keywords as keys"),
            Self::StringEscape => write!(f, "bad string escape sequence"),
            Self::StringMismatchedDoubleQuote => write!(f, "unbalanced double quotes in string"),
            Self::NonNumericArguments => write!(f, "arguments have to be numeric"),
            Self::SymbolNotFound(sym) => write!(f, "symbol '{}' not found", sym),
        }
    }
}

pub type Result<T> = std::result::Result<T, MalError>;
pub type MalResult = Result<super::types::MalType>;
