pub const MalError = error {
    SystemError,
    EnvLookupError,
    ApplyError,
    EvalError,
    KeyError,
    ThrownError,
    TypeError,
    ArgError,
    ReaderUnmatchedParen,
    ReaderUnmatchedString,
    ReaderBadHashmap,
    OutOfBounds,
    Overflow,
    DivisionByZero,
};

pub fn error_string_repr(mal_error: MalError) []const u8 {
    return @errorName(mal_error);
}
