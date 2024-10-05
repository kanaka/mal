include "reader";
include "printer";
include "utils";
include "env";

def READ:
    read_form;

# Environment Functions

def env_set(env; $key; $value):
    {
        parent: env.parent,
        environment: (env.environment + (env.environment | .[$key] |= $value)) # merge together, as .environment[key] |= value does not work
    };

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
        | {expr:., env:env}
    ) //
        jqmal_error("Unsupported native function kind \(.kind)");

def EVAL(env):
            if "DEBUG-EVAL" | env_get(env) |
                . != null and .kind != "false" and .kind != "nil"
            then
                ("EVAL: \(pr_str(env))" | _display | empty), .
            end
            |
            (select(.kind == "list") |
                .value | select(length != 0) |
                            (
                                select(.[0].value == "def!") |
                                    .[1].value as $key |
                                    .[2] | EVAL(env) |
                                    .expr as $value |
                                    .env |= env_set(.; $key; $value)
                            ) //
                            (
                                select(.[0].value == "let*") |
                                        (reduce (.[1].value | nwise(2)) as $xvalue (
                                            # Initial accumulator
                                            {parent:env, environment:{}, fallback:null};
                                            # Loop body
                                            . as $env | $xvalue[1] | EVAL($env) |
                                            env_set(.env; $xvalue[0].value; .expr)
                                        )) as $env |
                                        .[2] | {expr:EVAL($env).expr, env:env}
                            ) //
                            (
                                reduce .[] as $elem (
                                    [];
                                    . as $dot | $elem | EVAL(env) as $eval_env |
                                        ($dot + [$eval_env.expr])
                                ) | { expr: ., env: env } as $ev
                                    | $ev.expr | first |
                                        interpret($ev.expr[1:]; $ev.env)
                            )
            ) //
            (
                select(.kind == "vector") |
                .value |
                reduce .[] as $x ({expr:[], env:env};
                    . as $acc |
                    $x | EVAL($acc.env) |
                    .expr |= $acc.expr + [.]
                ) |
               .expr |= {kind:"vector", value:.}
            ) //
            (
                select(.kind == "hashmap") |
                .value | to_entries |
                 reduce .[] as $x ({expr:[], env:env};
                    . as $acc |
                    $x.value.value | EVAL($acc.env) |
                    .expr |= (. as $e | $acc.expr + [$x | .value.value |= $e])
                ) |
                .expr |= {kind:"hashmap", value:from_entries}
            ) //
            (
                select(.kind == "symbol") |
                .value |
                env_get(env) // jqmal_error("'\(.)' not found") |
                {expr:., env:env}
            ) //
            {expr:., env:env};

def PRINT:
    pr_str;

def repl:
    # Infinite generator, interrupted by an exception or ./run.
    . as $env | "user> " | __readline |
    try (
        READ | EVAL($env) |
        (.expr | PRINT), (.env | repl)
    ) catch if is_jqmal_error then
        ., ($env | repl)
    else
        halt_error
    end;

# The main program starts here.
    {
        parent: null,
        environment: {
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
    }
    |
        repl
