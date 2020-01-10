include "reader";
include "printer";
include "utils";
include "interp";
include "env";
include "core";

def read_line:
    . as $in
    | label $top
    | _readline;

def READ:
    read_str | read_form | .value;

def recurseflip(x; y):
    recurse(y; x);

def TCOWrap(env; retenv; continue):
    {
        ast: .,
        env: env,
        ret_env: retenv,
        finish: (continue | not),
        cont: true # set inside
    };

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
    . as $ast
    | { env: env, ast: ., cont: true, finish: false, ret_env: null }
    | [ recurseflip(.cont;
        .env as $_menv
        | if .finish then
            .cont |= false
        else
            (.ret_env//.env) as $_retenv
            | .ret_env as $_orig_retenv
            | .ast
            |
            (select(.kind == "list") |
                if .value | length == 0 then 
                    . | TCOWrap($_menv; $_orig_retenv; false)
                else
                    (
                        (
                            .value | select(.[0].value == "def!") as $value |
                                ($value[2] | EVAL($_menv)) as $evval |
                                    addToEnv($evval; $value[1].value) as $val |
                                    $val.expr | TCOWrap($val.env; $_orig_retenv; false)
                        ) //
                        (
                            .value | select(.[0].value == "let*") as $value |
                                ($_menv | pureChildEnv) as $subenv |
                                    (reduce ($value[1].value | nwise(2)) as $xvalue (
                                        $subenv;
                                        . as $env | $xvalue[1] | EVAL($env) as $expenv |
                                            env_set($expenv.env; $xvalue[0].value; $expenv.expr))) as $env
                                                | $value[2] | TCOWrap($env; $_retenv; true)
                        ) //
                        (
                            .value | select(.[0].value == "do") as $value |
                                (reduce ($value[1:][]) as $xvalue (
                                    { env: $_menv, expr: {kind:"nil"} };
                                    .env as $env | $xvalue | EVAL($env)
                                )) | . as $ex | .expr | TCOWrap($ex.env; $_orig_retenv; false)
                        ) //
                        (
                            .value | select(.[0].value == "if") as $value |
                                $value[1] | EVAL(env) as $condenv |
                                    (if (["false", "nil"] | contains([$condenv.expr.kind])) then
                                        ($value[3] // {kind:"nil"})
                                    else
                                        $value[2]
                                    end) | TCOWrap($condenv.env; $_orig_retenv; true)
                        ) //
                        (
                            .value | select(.[0].value == "fn*") as $value |
                                # we can't do what the guide says, so we'll skip over this
                                # and ues the later implementation
                                # (fn* args body)
                                $value[1].value | map(.value) as $binds | {
                                    kind: "function",
                                    binds: $binds,
                                    env: $_menv,
                                    body: $value[2],
                                    names: [], # we can't do that circular reference this
                                    free_referencess: $value[2] | find_free_references($_menv | env_dump_keys + $binds) # for dynamically scoped variables
                                } | TCOWrap($_menv; $_orig_retenv; false)
                        ) //
                        (
                            reduce .value[] as $elem (
                                [];
                                . as $dot | $elem | EVAL($_menv) as $eval_env |
                                    ($dot + [$eval_env.expr])
                            ) | . as $expr | first |
                                    interpret($expr[1:]; $_menv; _eval_here) as $exprenv |
                                    $exprenv.expr | TCOWrap($exprenv.env; $_orig_retenv; false)
                        ) //
                            TCOWrap($_menv; $_orig_retenv; false)
                    )
                end
            ) //
            (select(.kind == "vector") |
                if .value|length == 0 then
                    {
                        kind: "vector",
                        value: []
                    } | TCOWrap($_menv; $_orig_retenv; false)
                else
                    [ { env: $_menv, list: .value } | map_with_env ] as $res |
                    {
                        kind: "vector",
                        value: $res | map(.value)
                    } | TCOWrap($res | last.env; $_orig_retenv; false)
                end
            ) //
            (select(.kind == "hashmap") |
                [ { env: $_menv, list: .value | to_entries } | hmap_with_env ] as $res |
                {
                    kind: "hashmap",
                    value: $res | map(.value) | from_entries
                } | TCOWrap($res | last.env; $_orig_retenv; false)
            ) //
            (select(.kind == "function") |
                . | TCOWrap($_menv; $_orig_retenv; false) # return this unchanged, since it can only be applied to
            ) //
            (select(.kind == "symbol") |
                .value | env_get($_menv) | TCOWrap($_menv; null; false)
            ) // TCOWrap($_menv; $_orig_retenv; false)
        end
    ) ]
    | last as $result
    | ($result.ret_env // $result.env) as $env
    | $result.ast
    | addEnv($env);

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
        } + core_identify),
        fallback: null
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

repl(
    "(def! not (fn* (a) (if a false true)))" | rep(replEnv) | .env
)