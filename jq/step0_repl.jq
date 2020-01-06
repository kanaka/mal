include "utils";

def read_line:
    . as $in
    | label $top
    | input;

def READ:
    .;

def EVAL:
    .;

def PRINT:
    .;

def rep:
    READ | EVAL | PRINT | _print;

def repl_:
    ("user> " | _print) |
    (read_line | rep);

def repl:
    while(true; repl_);

repl
