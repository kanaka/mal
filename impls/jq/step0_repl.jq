include "utils";

def read_line:
    . as $in
    | label $top
    | _readline;

def READ:
    .;

def EVAL:
    .;

def PRINT:
    .;

def rep:
    READ | EVAL | PRINT | _display;

def repl_:
    ("user> " | _print) |
    (read_line | rep);

def repl:
    while(true; repl_);

repl
