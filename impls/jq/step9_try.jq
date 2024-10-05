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

def quasiquote:

    # If input is ('name, arg), return arg, else nothing.
    def _starts_with(name):
        select(.kind == "list")
        | .value
        | select(length == 2)
        | select(.[0] | .kind == "symbol" and .value == name)
        | .[1];

    # Right-folding function. The current element is provided as input.
    def qq_loop(acc):
        (_starts_with("splice-unquote")
         | {kind:"list", value:[{kind:"symbol", value:"concat"}, ., acc]})
        // {kind:"list", value:[{kind:"symbol", value:"cons"}, quasiquote, acc]};

    # Adapt parameters for jq foldr.
    def qq_foldr:
        .value
        | reverse
        | reduce .[] as $elt ({kind:"list", value:[]};
                              . as $acc | $elt | qq_loop($acc));

    _starts_with("unquote")
    // (
        select(.kind == "list")
        | qq_foldr
    ) // (
        select(.kind == "vector")
        | {kind:"list", value: [{kind:"symbol", value:"vec"}, qq_foldr]}
    ) // (
        select(.kind == "hashmap" or .kind == "symbol")
        | {kind:"list", value:[{kind:"symbol", value:"quote"}, .]}
    ) // .;

def set_macro_function:
    if .kind != "function" then
        jqmal_error("expected a function to be defined by defmacro!")
    else
        .is_macro |= true
    end;

def EVAL(env):
    def _eval_here:
        .env as $env | .expr | EVAL($env);

    . as $ast
    | TCOWrap(env; null; true)
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
            if "DEBUG-EVAL" | env_get($currentEnv) |
                . != null and .kind != "false" and .kind != "nil"
            then
                ("EVAL: \(pr_str(env))" | _display | empty), .
            end
            |
            (select(.kind == "list") |
                .value | select(length != 0) as $value |
                            (
                                select(.[0].value == "def!") |
                                    $value[2] | EVAL($_menv) |
                                        addToEnv($value[1].value) as $val |
                                        $val.expr | TCOWrap($val.env; $_orig_retenv; false)
                            ) //
                            (
                                select(.[0].value == "defmacro!") |
                                    $value[2] | EVAL($_menv) |
                                        .expr |= set_macro_function |
                                        addToEnv($value[1].value) as $val |
                                        $val.expr | TCOWrap($val.env; $_orig_retenv; false)
                            ) //
                            (
                                select(.[0].value == "let*") |
                                        (reduce ($value[1].value | nwise(2)) as $xvalue (
                                            # Initial accumulator
                                            {parent:$currentEnv, environment:{}, fallback:null} |
                                            wrapEnv($replEnv; $_menv.atoms);
                                            # Loop body
                                            . as $env | $xvalue[1] | EVAL($env) as $expenv |
                                                env_set_($expenv.env; $xvalue[0].value; $expenv.expr))) as $env
                                                    | $value[2] | TCOWrap($env; $_retenv; true)
                            ) //
                            (
                                select(.[0].value == "do") |
                                    (reduce $value[1:-1][] as $xvalue (
                                        $_menv;
                                        . as $env | $xvalue | EVAL($env) | .env
                                    )) as $env |
                                    $value[-1] | TCOWrap($env; $_orig_retenv; true)
                            ) //
                            (
                                select(.[0].value == "try*") |
                                  if $value[2]
                                  and ($value[2].value[0] | .kind == "symbol" and .value == "catch*")
                                  then
                                    try (
                                        $value[1] | EVAL($_menv) as $exp | $exp.expr | TCOWrap($exp.env; $_orig_retenv; false)
                                    ) catch ( . as $exc |
                                                (if ($exc | is_jqmal_error) then
                                                    $exc[19:] as $ex |
                                                        try (
                                                            $ex 
                                                            | fromjson
                                                        ) catch (
                                                            $ex |
                                                            wrap("string")
                                                        )
                                                else 
                                                    $exc|wrap("string")
                                                end) as $exc |
                                                $value[2].value[2] | EVAL($currentEnv | childEnv([$value[2].value[1].value]; [$exc]) | wrapEnv($replEnv; $_menv.atoms)) as $ex |
                                                $ex.expr | TCOWrap($ex.env; $_retenv; false)
                                    )
                                  else
                                      $value[1] | EVAL($_menv) as $exp |
                                      $exp.expr | TCOWrap($exp.env; $_orig_retenv; false)
                                  end
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
                                        is_macro: false
                                } | TCOWrap($_menv; $_orig_retenv; false)
                            ) //
                            (
                                select(.[0].value == "quote") |
                                    $value[1] | TCOWrap($_menv; $_orig_retenv; false)
                            ) //
                            (
                                select(.[0].value == "quasiquote") |
                                    $value[1] | quasiquote | TCOWrap($_menv; $_orig_retenv; true)
                            ) //
                            (
                                (
                                    .[0] | EVAL($_menv) |
                                    (.env | setpath(["atoms"]; $_menv.atoms)) as $_menv |
                                    .expr
                                ) as $fn |
                                if $fn.kind == "function" and $fn.is_macro then
                                    $fn | interpret($value[1:]; $_menv; _eval_here) as $exprenv |
                                    $exprenv.expr | TCOWrap($exprenv.env; $_orig_retenv; true)
                                else
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
                                end
                            )
            ) //
            (
                select(.kind == "vector") |
                .value |
                reduce .[] as $x ({expr:[], env:$_menv};
                    . as $acc |
                    $x | EVAL($acc.env) |
                    .expr |= $acc.expr + [.]
                ) |
                .env as $e |
               {kind:"vector", value:.expr} |
               TCOWrap($e; $_orig_retenv; false)
            ) //
            (
                select(.kind == "hashmap") |
                .value | to_entries |
                 reduce .[] as $x ({expr:[], env:$_menv};
                    . as $acc |
                    $x.value.value | EVAL($acc.env) |
                    .expr |= (. as $e | $acc.expr + [$x | .value.value |= $e])
                ) |
                .env as $e |
                {kind:"hashmap", value:.expr|from_entries} |
                TCOWrap($e; $_orig_retenv; false)
            ) //
            (
                select(.kind == "function") |
                . | TCOWrap($_menv; $_orig_retenv; false) # return this unchanged, since it can only be applied to
            ) //
            (
                select(.kind == "symbol") |
                .value |
                env_get($currentEnv) // jqmal_error("'\(.)' not found") |
                TCOWrap($_menv; $_orig_retenv; false)
            ) //
            TCOWrap($_menv; $_orig_retenv; false)
        end
    ) ] |
    last |
    {expr: .ast, env:(.ret_env // .env)};

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
    | eval_ign("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")
    | env_set_(.; "*ARGV*"; {kind:"list", value:[$ARGS.positional[1:] | .[] | {kind:"string", value:.}]})
    |
    if $ARGS.positional|length > 0 then
        eval_ign("(load-file \($ARGS.positional[0] | tojson))") |
        empty
    else
        repl
    end
