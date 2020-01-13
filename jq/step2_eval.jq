include "reader";
include "printer";
include "utils";

def read_line:
    . as $in
    | label $top
    | _readline;

def READ:
    read_str | read_form | .value;

def lookup(env):
    env[.] //
            jqmal_error("'\(.)' not found");

def arg_check(args):
    if .inputs != (args|length) then
        jqmal_error("Invalid number of arguments (expected \(.inputs), got \(args|length))")
    else
        .
    end;

def interpret(arguments; env):
    (select(.kind == "fn") |
        arg_check(arguments) |
        (
            select(.function == "number_add") |
            arguments | map(.value) | .[0] + .[1] | wrap("number")
        ) // (
            select(.function == "number_sub") |
            arguments | map(.value) | .[0] - .[1] | wrap("number")
        ) // (
            select(.function == "number_mul") |
            arguments | map(.value) | .[0] * .[1] | wrap("number")
        ) // (
            select(.function == "number_div") |
            arguments | map(.value) | .[0] / .[1] | wrap("number")
        )
    ) //
        jqmal_error("Unsupported native function kind \(.kind)");

def EVAL(env):
    def eval_ast:
        (select(.kind == "symbol") | .value | lookup(env)) //
        (select(.kind == "list")   | {
            kind: "list",
            value: .value | map(EVAL(env))
        }) // .;
    (select(.kind == "list") |
        if .value | length == 0 then 
            .
        else
            eval_ast|.value as $evald | $evald | first |  interpret($evald[1:]; env)
        end
    ) //
    (select(.kind == "vector") |
        {
            kind: "vector",
            value: .value|map(EVAL(env))
        }
    ) //
    (select(.kind == "hashmap") |
        {
            kind: "hashmap",
            value: .value|map_values(.value |= EVAL(env))
        }
    ) // eval_ast;

def PRINT:
    pr_str;

def rep(env):
    READ | EVAL(env) |
        if . != null then
            PRINT
        else
            null
        end;

def repl_(env):
    ("user> " | _print) |
    (read_line | rep(env));

# we don't have no indirect functions, so we'll have to interpret the old way
def replEnv:
    {
        "+": {
            kind: "fn", # native function
            inputs: 2,
            function: "number_add"
        },
        "-": {
            kind: "fn", # native function
            inputs: 2,
            function: "number_sub"
        },
        "*": {
            kind: "fn", # native function
            inputs: 2,
            function: "number_mul"
        },
        "/": {
            kind: "fn", # native function
            inputs: 2,
            function: "number_div"
        },
    };

def repl(env):
    {continue: true} | while(
        .continue;
        try {value: repl_(env), continue: true}
        catch
            if is_jqmal_error then
                {value: "Error: \(.)", continue: true}
            else
                {value: ., continue: false}
            end) | if .value then .value|_display else empty end;

repl(replEnv)