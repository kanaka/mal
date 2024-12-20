include "reader";
include "printer";
include "utils";

def READ:
    read_form;

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
            # ("EVAL: \(pr_str(env))" | _display | empty),
            (select(.kind == "list") |
                .value | select(length != 0) as $value |
                map(EVAL(env)) | .[1:] as $args | first | interpret($args; env)
            ) //
            (
                select(.kind == "vector") |
                    {
                        kind: "vector",
                        value: .value|map(EVAL(env))
                    }
            ) //
            (
                select(.kind == "hashmap") |
                    {
                        kind: "hashmap",
                        value: .value|map_values(.value |= EVAL(env))
                    }
            ) //
            (
                select(.kind == "symbol") |
                    env[.value] // jqmal_error("'\(.)' not found")
            ) //
            .;

def PRINT:
    pr_str;

def repl:
    # Infinite generator, interrupted by an exception or ./run.
    . as $env | "user> " | __readline |
    try (
        READ | EVAL($env) |
        PRINT, ($env | repl)
    ) catch if is_jqmal_error then
        ., ($env | repl)
    else
        halt_error
    end;

# The main program starts here.
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
    }
    |
        repl
