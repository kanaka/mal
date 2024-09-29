include "reader";
include "printer";
include "utils";
include "interp";
include "env";
include "core";

def READ:
    read_form;

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
            if "DEBUG-EVAL" | env_find($currentEnv).environment["DEBUG-EVAL"] |
                . != null and .kind != "false" and .kind != "nil"
            then
                ("EVAL: \(pr_str(env))" | _display | empty), .
            end
            |
            (select(.kind == "list") |
                .value | select(length != 0) as $value |
                        (
                            (
                                select(.[0].value == "def!") |
                                    ($value[2] | EVAL($_menv)) as $evval |
                                        addToEnv($evval; $value[1].value) as $val |
                                        $val.expr | TCOWrap($val.env; $_orig_retenv; false)
                            ) //
                            (
                                select(.[0].value == "let*") |
                                        ($currentEnv | pureChildEnv | wrapEnv($replEnv; $_menv.atoms)) as $_menv |
                                        (reduce ($value[1].value | nwise(2)) as $xvalue (
                                            $_menv;
                                            . as $env | $xvalue[1] | EVAL($env) as $expenv |
                                                env_set_($expenv.env; $xvalue[0].value; $expenv.expr))) as $env
                                                    | $value[2] | TCOWrap($env; $_retenv; true)
                            ) //
                            (
                                select(.[0].value == "do") |
                                    (reduce ($value[1:][]) as $xvalue (
                                        { env: $_menv, expr: {kind:"nil"} };
                                        .env as $env | $xvalue | EVAL($env)
                                    )) | . as $ex | .expr | TCOWrap($ex.env; $_orig_retenv; false)
                            ) //
                            (
                                select(.[0].value == "if") |
                                    $value[1] | EVAL($_menv) as $condenv |
                                        (if (["false", "nil"] | contains([$condenv.expr.kind])) then
                                            ($value[3] // {kind:"nil"})
                                        else
                                            $value[2]
                                        end) | TCOWrap($condenv.env; $_orig_retenv; true)
                            ) //
                            (
                                select(.[0].value == "fn*") |
                                    # (fn* args body)
                                    $value[1].value | map(.value) as $binds | 
                                    ($value[2] | find_free_references($currentEnv | env_dump_keys + $binds)) as $free_referencess | {
                                        kind: "function",
                                        binds: $binds,
                                        env: ($_menv | env_remove_references($free_referencess)),
                                        body: $value[2],
                                        names: [], # we can't do that circular reference thing
                                        free_referencess: $free_referencess,  # for dynamically scoped variables
                                } | TCOWrap($_menv; $_orig_retenv; false)
                            ) //
                            (
                                (
                                    .[0] | EVAL($_menv) |
                                    (.env | setpath(["atoms"]; $_menv.atoms)) as $_menv |
                                    .expr
                                ) as $fn |
                                    $value[1:] |
                                    (reduce .[] as $elem (
                                        {env: $_menv, val: []};
                                        # debug(".val: \(.val)     elem=\($elem)") |
                                        . as $dot | $elem | EVAL($dot.env) as $eval_env |
                                            ($dot.env | setpath(["atoms"]; $eval_env.env.atoms)) as $_menv |
                                            {env: $_menv, val: ($dot.val + [$eval_env.expr])}
                                            # | debug(".val: \(.val)")
                                    )) as $expr |
                                        # debug("fn.kind: \($fn.kind)", "expr: \($expr)") |
                                            $fn |
                                            interpret($expr.val; $expr.env; _eval_here) as $exprenv |
                                        $exprenv.expr | TCOWrap($exprenv.env; $_orig_retenv; false)
                            ) //
                                TCOWrap($_menv; $_orig_retenv; false)
                        )
            ) //
            (select(.kind == "vector") |
                if .value|length == 0 then
                    {
                        kind: "vector",
                        value: []
                    } | TCOWrap($_menv; $_orig_retenv; false)
                else
                    [ { env: $currentEnv, list: .value } | map_with_env ] as $res |
                    {
                        kind: "vector",
                        value: $res | map(.value)
                    } | TCOWrap($res | last.env; $_orig_retenv; false)
                end
            ) //
            (
                select(.kind == "hashmap") |
                [ { env: $currentEnv, list: (.value | to_entries) } | hmap_with_env ] as $res |
                {
                    kind: "hashmap",
                    value: $res | map(.value) | from_entries
                } | TCOWrap($res | last.env; $_orig_retenv; false)
            ) //
            (
                select(.kind == "function") |
                . | TCOWrap($_menv; $_orig_retenv; false) # return this unchanged, since it can only be applied to
            ) //
            (
                select(.kind == "symbol") |
                .value | env_get($currentEnv) | TCOWrap($_menv; null; false)
            ) //
            TCOWrap($_menv; $_orig_retenv; false)
        end
    ) ] 
    | last as $result
    | ($result.ret_env // $result.env) as $env
    | $result.ast
    | addEnv($env);

def PRINT(env):
    pr_str(env);

def repl:
    # Infinite generator, interrupted by an exception or ./run.
    . as $env | "user> " | __readline |
    try (
        READ | EVAL($env) | .env as $env |
        (.expr | PRINT($env)), ($env | repl)
    ) catch if is_jqmal_error then
        ., ($env | repl)
    else
        halt_error
    end;

def eval_ign(expr):
    . as $env | expr | READ | EVAL($env) | .env;

# The main program starts here.
    {
        parent: null,
        environment: core_identify,
        fallback: null
    }
    | wrapEnv({})
    | eval_ign("(def! not (fn* (a) (if a false true)))")
    | eval_ign("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\\nnil)\")))))))")
    | env_set_(.; "*ARGV*"; {kind:"list", value:[$ARGS.positional[1:] | .[] | {kind:"string", value:.}]})
    |
    if $ARGS.positional|length > 0 then
        eval_ign("(load-file \($ARGS.positional[0] | tojson))") |
        empty
    else
        repl
    end
