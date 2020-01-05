include "reader";
include "printer";

def read_line:
    . as $in
    | label $top
    | input;

def READ:
    read_str;

def EVAL:
    . | read_form | .value;

def PRINT:
    . | pr_str;

def rep:
    . | READ | EVAL | PRINT;

def repl_:
    [
        ("user> " | stderr),
        (read_line | rep)
    ] | last;

def repl:
    while(true; try repl_ catch "Error: \(.)");

repl
