include "reader";
include "printer";
include "utils";

def read_line:
    . as $in
    | label $top
    | _readline;

def READ:
    read_str | read_form | .value;

# Environment functions

def pureChildEnv:
    {
        parent: .,
        environment: {}
    };

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

def addEnv(env):
    {
        expr: .,
        env: env
    };
    
# Evaluation

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
    def hmap_with_env:
        .env as $env | .list as $list |
            if $list|length == 0 then
                empty
            else
                $list[0] as $elem |
                $list[1:] as $rest |
                    $elem[1] | EVAL($env) as $resv |
                        { value: [$elem[0], $resv.expr], env: env },
                        ({env: $resv.env, list: $rest} | hmap_with_env)
            end;
    def map_with_env:
        .env as $env | .list as $list |
            if $list|length == 0 then
                empty
            else
                $list[0] as $elem |
                $list[1:] as $rest |
                    $elem | EVAL($env) as $resv |
                        { value: $resv.expr, env: env },
                        ({env: $resv.env, list: $rest} | map_with_env)
            end;
    (select(.kind == "list") |
        if .value | length == 0 then 
            . | addEnv(env)
        else
            (
                (
                    .value | select(.[0].value == "def!") as $value |
                        ($value[2] | EVAL(env)) as $evval |
                            addToEnv($evval; $value[1].value)
                ) //
                (
                    .value | select(.[0].value == "let*") as $value |
                        (env | pureChildEnv) as $subenv |
                            (reduce ($value[1].value | nwise(2)) as $xvalue (
                                $subenv;
                                . as $env | $xvalue[1] | EVAL($env) as $expenv |
                                    env_set($expenv.env; $xvalue[0].value; $expenv.expr))) as $env
                                        | $value[2] | { expr: EVAL($env).expr, env: env }
                ) //
                (
                    reduce .value[] as $elem (
                        [];
                        . as $dot | $elem | EVAL(env) as $eval_env |
                            ($dot + [$eval_env.expr])
                    ) | { expr: ., env: env } as $ev
                        | $ev.expr | first |
                            interpret($ev.expr[1:]; $ev.env)
                ) //
                    addEnv(env)
            )
        end
    ) //
    (select(.kind == "vector") |
        [ { env: env, list: .value } | map_with_env ] as $res |
        {
            kind: "vector",
            value: $res | map(.value)
        } | addEnv($res | last.env)
    ) //
    (select(.kind == "hashmap") |
        [ { env: env, list: .value | to_entries } | hmap_with_env ] as $res |
        {
            kind: "hashmap",
            value: $res | map(.value) | from_entries
        } | addEnv($res | last.env)
    ) //
    (select(.kind == "symbol") |
        .value | env_get(env) | addEnv(env)
    ) // addEnv(env);

def PRINT:
    pr_str;

def rep(env):
    READ | EVAL(env) as $expenv |
        if $expenv.expr != null then
            $expenv.expr | PRINT
        else
            null
        end | addEnv($expenv.env);

def repl_(env):
    ("user> " | _print) |
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
    };

def repl(env):
    def xrepl:
        (.env as $env | try repl_($env) catch addEnv($env)) as $expenv |
            {
                value: $expenv.expr,
                stop: false,
                env: ($expenv.env // .env)
            } | ., xrepl;
    {stop: false, env: env} | xrepl | if .value then (.value | _display) else empty end;

repl(replEnv)
