include "reader";
include "printer";
include "utils";
include "interp";

def read_line:
    . as $in
    | label $top
    | input;

def READ:
    read_str | read_form | .value;

def lookup(env):
    env.environment[.] //
        if env.parent then
            lookup(env.parent)
        else
            jqmal_error("Symbol \(.) not found")
        end;

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
    ("user> " | stderr) |
    (read_line | rep(env));

def childEnv(binds; value):
    {
        parent: .,
        environment: [binds, value] | transpose | map({(.[0]): .[1]}) | from_entries
    };

# we don't have no indirect functions, so we'll have to interpret the old way
def replEnv:
    {
        parent: null,
        environment: {
            "+": {
                inputs: 2,
                function: "number_add"
            },
            "-": {
                inputs: 2,
                function: "number_sub"
            },
            "*": {
                inputs: 2,
                function: "number_mul"
            },
            "/": {
                inputs: 2,
                function: "number_div"
            },
        }
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
            end) | if .value then .value else empty end;

repl(replEnv)