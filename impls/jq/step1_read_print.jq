include "reader";
include "printer";
include "utils";

def READ:
    read_form;

def EVAL:
    .;

def PRINT:
    pr_str;

def repl:
    # Infinite generator, interrupted by an exception or ./run.
    "user> " | __readline |
    try (
        READ | EVAL |
        PRINT, repl
    ) catch if is_jqmal_error then
        ., repl
    else
        halt_error
    end;

repl
