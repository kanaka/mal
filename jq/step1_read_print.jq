include "reader";
include "printer";
include "utils";

def read_line:
    . as $in
    | label $top
    | _readline;

def READ:
    read_str | read_form | .value;

def EVAL:
    .;

def PRINT:
    pr_str;

def rep:
    READ | EVAL |
        if . != null then
            PRINT
        else
            null
        end;

def repl_:
    ("user> " | _print) |
    (read_line | rep);

def repl:
    {continue: true} | while(
        .continue;
        try {value: repl_, continue: true}
        catch
            if is_jqmal_error then
                {value: "Error: \(.)", continue: true}
            else
                {value: ., continue: false}
            end) | if .value then .value|_display else empty end;

repl
