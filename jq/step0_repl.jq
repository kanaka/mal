
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
    READ | EVAL | PRINT;

def repl_:
    ("user> " | stderr) |
    (read_line | rep);

def repl:
    while(true; repl_);

repl
