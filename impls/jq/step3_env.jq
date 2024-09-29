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

def env_find(env):
    if env.environment[.] == null then
        if env.parent then
            env_find(env.parent)
        else
            null
        end
    else
        env
    end;

def addToEnv(envexp; name):
    {
        expr: envexp.expr,
        env: env_set(envexp.env; name; envexp.expr)
    };

def env_get(env):
    . as $key | $key | env_find(env).environment[$key] as $value |
    if $value == null then
        jqmal_error("'\($key)' not found")
    else
        $value
    end;

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
    ) | addEnv(env) //
        jqmal_error("Unsupported native function kind \(.kind)");

def EVAL(env):
            if "DEBUG-EVAL" | env_find(env).environment["DEBUG-EVAL"] |
                . != null and .kind != "false" and .kind != "nil"
            then
                ("EVAL: \(pr_str(env))" | _display | empty), .
            end
            |
            (select(.kind == "list") |
                .value | select(length != 0) as $value |
                        (
                            (
                                select(.[0].value == "def!") as $value |
                                    ($value[2] | EVAL(env)) as $evval |
                                        addToEnv($evval; $value[1].value)
                            ) //
                            (
                                select(.[0].value == "let*") as $value |
                                    (env | pureChildEnv) as $subenv |
                                        (reduce ($value[1].value | nwise(2)) as $xvalue (
                                            $subenv;
                                            . as $env | $xvalue[1] | EVAL($env) as $expenv |
                                                env_set($expenv.env; $xvalue[0].value; $expenv.expr))) as $env
                                                    | $value[2] | { expr: EVAL($env).expr, env: env }
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
                .value | env_get(env) | addEnv(env)
            ) //
            addEnv(env);

def PRINT:
    pr_str;

def childEnv(binds; value):
    {
        parent: .,
        environment: [binds, value] | transpose | map({(.[0]): .[1]}) | from_entries
    };

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
