MODULE REPL;

FROM DynamicStrings IMPORT String;

PROCEDURE READ(String input): String;
BEGIN
    RETURN input;
END READ;

PROCEDURE EVAL(String input): String;
BEGIN
    RETURN input;
END EVAL;

PROCEDURE PRINT(String input): String;
BEGIN
    RETURN input;
END PRINT;

PROCEDURE rep(String input): String;
BEGIN
    RETURN PRINT(EVAL(READ(input)));
END rep;

END REPL.