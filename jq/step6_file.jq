include "reader";
include "printer";
include "utils";
include "interp";
include "env";
include "core";

def read_line:
    . as $in
    | label $top
    | input;

def READ:
    read_str | read_form | .value;

# def eval_ast(env):
#         (select(.kind == "symbol") | .value | env_get(env) | addEnv(env)) //
#         (select(.kind == "list") | reduce .value[] as $elem (
#             {value: [], env: env};
#             . as $dot | $elem | EVAL($dot.env) as $eval_env |
#                 {
#                     value: ($dot.value + [$eval_env.expr]),
#                     env: $eval_env.env
#                 }
#         ) | { expr: .value, env: .env }) // (addEnv(env));

# def patch_with_env(env):
#     . as $dot | (reduce .[] as $fnv (
#         [];
#         . + [$fnv | setpath([1, "free_referencess"]; ($fnv[1].free_referencess + $dot) | unique)]
#     )) as $functions | reduce $functions[] as $function (
#         env;
#         env_set(.; $function[0]; $function[1])
#     ) | { functions: $functions, env: . };

def find_free_references(keys):
    def _refs:
        . as $dot
        | if .kind == "symbol" then
            if keys | contains([$dot.value]) then [] else [$dot.value] end
        else if "list" == $dot.kind then
            ($dot.value[1:] | map(_refs) | reduce .[] as $x ([]; . + $x)) + ($dot.value[0] | find_free_references(keys + ["if", "def!", "let*", "fn*"]))
        else if "vector" == $dot.kind then
            ($dot.value[1:] | map(_refs) | reduce .[] as $x ([]; . + $x)) + ($dot.value[0] | find_free_references(keys + ["if", "def!", "let*", "fn*"]))
        else
            []
        end end end;
    _refs | unique;

def EVAL(env):
    def _eval_here:
        .env as $env | .expr | EVAL($env);

    def hmap_with_env:
        .env as $env | .list as $list |
            if $list|length == 0 then
                empty
            else
                $list[0] as $elem |
                $list[1:] as $rest |
                    $elem.value.value | EVAL($env) as $resv |
                        {
                            value: {
                                key: $elem.key,
                                value: { kkind: $elem.value.kkind, value: $resv.expr }
                            },
                            env: env
                        },
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
    . as $init |
    env | unwrapCurrentEnv as $currentEnv | # unwrap env "package"
    env | unwrapReplEnv    as $replEnv    | # -
    $init |
    (select(.kind == "list") |
        if .value | length == 0 then 
            .
        else
            (
                (
                    .value | select(.[0].value == "def!") as $value |
                        ($value[2] | EVAL(env)) as $evval |
                            addToEnv($evval; $value[1].value)
                ) //
                (
                    .value | select(.[0].value == "let*") as $value |
                        ($currentEnv | pureChildEnv | wrapEnv($replEnv)) as $subenv |
                            (reduce ($value[1].value | nwise(2)) as $xvalue (
                                $subenv;
                                . as $env | $xvalue[1] | EVAL($env) as $expenv |
                                    env_set_($expenv.env; $xvalue[0].value; $expenv.expr))) as $env
                                        | $value[2] | { expr: EVAL($env).expr, env: env }
                ) //
                (
                    .value | select(.[0].value == "do") as $value |
                        (reduce ($value[1:][]) as $xvalue (
                            { env: env, expr: {kind:"nil"} };
                            .env as $env | $xvalue | EVAL($env)
                        ))
                ) //
                (
                    .value | select(.[0].value == "if") as $value |
                        $value[1] | EVAL(env) as $condenv |
                            if (["false", "nil"] | contains([$condenv.expr.kind])) then
                                ($value[3] // {kind:"nil"}) | EVAL($condenv.env)
                            else
                                $value[2] | EVAL($condenv.env)
                            end
                ) //
                (
                    .value | select(.[0].value == "fn*") as $value |
                        # we can't do what the guide says, so we'll skip over this
                        # and ues the later implementation
                        # (fn* args body)
                        $value[1].value | map(.value) as $binds | {
                            kind: "function",
                            binds: $binds,
                            env: env,
                            body: $value[2],
                            names: [], # we can't do that circular reference this
                            free_referencess: $value[2] | find_free_references($currentEnv | env_dump_keys + $binds) # for dynamically scoped variables
                        } | addEnv(env)
                ) //
                (
                    reduce .value[] as $elem (
                        [];
                        . as $dot | $elem | EVAL(env) as $eval_env |
                            ($dot + [$eval_env.expr])
                    ) | { expr: ., env: env } as $ev
                        | $ev.expr | first |
                            interpret($ev.expr[1:]; $ev.env; _eval_here)
                ) //
                    addEnv(env)
            )
        end
    ) //
    (select(.kind == "vector") |
        if .value|length == 0 then
            {
                kind: "vector",
                value: []
            } | addEnv(env)
        else
            [ { env: env, list: .value } | map_with_env ] as $res |
            {
                kind: "vector",
                value: $res | map(.value)
            } | addEnv($res | last.env)
        end
    ) //
    (select(.kind == "hashmap") |
        [ { env: env, list: (.value | to_entries) } | hmap_with_env ] as $res |
        {
            kind: "hashmap",
            value: $res | map(.value) | from_entries
        } | addEnv($res | last.env)
    ) //
    (select(.kind == "function") |
        . | addEnv(env) # return this unchanged, since it can only be applied to
    ) //
    (select(.kind == "symbol") |
        .value | env_get($currentEnv) | addEnv(env)
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

# we don't have no indirect functions, so we'll have to interpret the old way
def replEnv:
    {
        parent: null,
        environment: ({
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
            "eval": {
                kind: "fn",
                inputs: 1,
                function: "eval"
            }
        } + core_identify)
    };

def repl(env):
    def xrepl:
        (.env as $env | try repl_($env) catch addEnv($env)) as $expenv |
            {
                value: $expenv.expr,
                stop: false,
                env: ($expenv.env // .env)
            } | ., xrepl;
    {stop: false, env: env} | xrepl | if .value then (.value | _print) else empty end;

def eval_ign(expr):
    . as $env | expr | rep($env) | .env;

repl(
    replEnv
    | wrapEnv
    | eval_ign("(def! not (fn* (a) (if a false true)))")
    | eval_ign("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\\nnil)\")))))))")
    | . as $env 
    | env_set_($env; "*ARGV*"; $ARGS.positional | wrap("list"))
)