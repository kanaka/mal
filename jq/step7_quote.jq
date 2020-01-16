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
        ret_env: (if retenv != null then (retenv | setpath(["atoms"]; env.atoms)) else retenv end),
        finish: (continue | not),
        cont: true # set inside
    };

def _symbol(name):
    {
        kind: "symbol",
        value: name
    };

def _symbol_v(name):
    if .kind == "symbol" then
        .value == name
    else
        false
    end;

def quasiquote:
    if isPair then
        .value as $value | null |
        if ($value[0] | _symbol_v("unquote")) then
            $value[1]
        else
            if isPair($value[0]) and ($value[0].value[0] | _symbol_v("splice-unquote")) then
                    [_symbol("concat")] +
                    [$value[0].value[1]] + 
                    [($value[1:] | wrap("list") | quasiquote)] | wrap("list")
            else
                    [_symbol("cons")] + 
                    [($value[0] | quasiquote)] +
                    [($value[1:] | wrap("list") | quasiquote)] | wrap("list")
            end
        end
    else
            [_symbol("quote")] + 
            [.] | wrap("list")
    end;

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
    . as $ast
    | { env: env, ast: ., cont: true, finish: false, ret_env: null }
    | [ recurseflip(.cont;
        .env as $_menv
        | if .finish then
            .cont |= false
        else
            (.ret_env//.env) as  $_retenv
            | .ret_env as $_orig_retenv
            | .ast
            | . as $init
            | $_menv | unwrapCurrentEnv as $currentEnv # unwrap env "package"
            | $_menv | unwrapReplEnv    as $replEnv    # -
            | $init
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
                                    ($currentEnv | pureChildEnv | wrapEnv($replEnv; $_menv.atoms)) as $_menv |
                                    (reduce ($value[1].value | nwise(2)) as $xvalue (
                                        $_menv;
                                        . as $env | $xvalue[1] | EVAL($env) as $expenv |
                                            env_set_($expenv.env; $xvalue[0].value; $expenv.expr))) as $env
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
                                $value[1] | EVAL($_menv) as $condenv |
                                    (if (["false", "nil"] | contains([$condenv.expr.kind])) then
                                        ($value[3] // {kind:"nil"})
                                    else
                                        $value[2]
                                    end) | TCOWrap($condenv.env; $_orig_retenv; true)
                        ) //
                        (
                            .value | select(.[0].value == "fn*") as $value |
                                # (fn* args body)
                                $value[1].value | map(.value) as $binds | 
                                ($value[2] | find_free_references($currentEnv | env_dump_keys + $binds)) as $free_referencess | {
                                    kind: "function",
                                    binds: $binds,
                                    env: ($_menv | env_remove_references($free_referencess)),
                                    body: $value[2],
                                    names: [], # we can't do that circular reference thing
                                    free_referencess: $free_referencess  # for dynamically scoped variables
                                } | TCOWrap($_menv; $_orig_retenv; false)
                        ) //
                        (
                            .value | select(.[0].value == "quote") as $value |
                                $value[1] | TCOWrap($_menv; $_orig_retenv; false)
                        ) //
                        (
                            .value | select(.[0].value == "quasiquote") as $value |
                                $value[1] | quasiquote | TCOWrap($_menv; $_orig_retenv; true)
                        ) //
                        (
                            reduce .value[] as $elem (
                                {env: $_menv, val: []};
                                . as $dot | $elem | EVAL($dot.env) as $eval_env |
                                    ($dot.env | setpath(["atoms"]; $eval_env.env.atoms)) as $_menv |
                                    {env: $_menv, val: ($dot.val + [$eval_env.expr])}
                            ) | . as $expr | $expr.val | first |
                                    interpret($expr.val[1:]; $expr.env; _eval_here) as $exprenv |
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
                    [ { env: env, list: .value } | map_with_env ] as $res |
                    {
                        kind: "vector",
                        value: $res | map(.value)
                    } | TCOWrap($res | last.env; $_orig_retenv; false)
                end
            ) //
            (select(.kind == "hashmap") |
                [ { env: env, list: (.value | to_entries) } | hmap_with_env ] as $res |
                {
                    kind: "hashmap",
                    value: $res | map(.value) | from_entries
                } | TCOWrap($res | last.env; $_orig_retenv; false)
            ) //
            (select(.kind == "function") |
                . | TCOWrap($_menv; $_orig_retenv; false) # return this unchanged, since it can only be applied to
            ) //
            (select(.kind == "symbol") |
                .value | env_get($currentEnv) | TCOWrap($_menv; null; false)
            ) // TCOWrap($_menv; $_orig_retenv; false)
        end
    ) ] 
    | last as $result
    | ($result.ret_env // $result.env) as $env
    | $result.ast
    | addEnv($env);

def PRINT(env):
    pr_str(env);

def rep(env):
    READ | EVAL(env) as $expenv |
        if $expenv.expr != null then
            $expenv.expr | PRINT($expenv.env)
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
    {stop: false, env: env} | xrepl | if .value then (.value | _display) else empty end;

def eval_ign(expr):
    . as $env | expr | rep($env) | .env;

def eval_val(expr):
    . as $env | expr | rep($env) | .expr;

def getEnv:
    replEnv
    | wrapEnv({})
    | eval_ign("(def! not (fn* (a) (if a false true)))")
    | eval_ign("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\\nnil)\")))))))");

def main:
    if $ARGS.positional|length > 0 then
        getEnv as $env |
        env_set_($env; "*ARGV*"; $ARGS.positional[1:] | map(wrap("string")) | wrap("list")) |
        eval_val("(load-file \($ARGS.positional[0] | tojson))") |
        ""
    else
        repl( getEnv as $env | env_set_($env; "*ARGV*"; [] | wrap("list")) )
    end;

[ main ] | _halt
