include "reader";
include "printer";
include "utils";
include "env";
include "core";

def READ:
    read_form;

# Environment Functions

def env_set(env; $key; $value):
    (if $value.kind == "function" or $value.kind == "atom" then
        # inform the function/atom of its names
        $value | (.names += [$key]) | (.names |= unique) |
        if $value.kind == "atom" then
            # check if the one we have is newer
            ($key | env_get(env)) as $ours |
            if $ours.last_modified > $value.last_modified then
                $ours
            else
                # update modification timestamp
                $value | .last_modified |= now
            end
        else
            .
        end
    else 
        $value
    end) as $value | {
        parent: env.parent,
        environment: ((env.environment // jqmal_error("Environment empty in \(env | keys)")) + (env.environment | .[$key] |= $value)), # merge together, as env.environment[key] |= value does not work
        fallback: env.fallback
    };

def _env_remove_references(refs):
    if . != null then
        {
            environment: (.environment | to_entries | map(select(.key as $key | refs | contains([$key]) | not)) | from_entries),
            parent: (.parent | _env_remove_references(refs)),
            fallback: (.fallback | _env_remove_references(refs))
        }
    else . end;

def env_remove_references(refs):
    . as $env 
    | if has("replEnv") then
        .currentEnv |= _env_remove_references(refs)
      else
        _env_remove_references(refs)
      end;

# Evaluation

def arg_check(args):
    if .inputs < 0 then
        if (abs(.inputs) - 1) > (args | length) then
            jqmal_error("Invalid number of arguments (expected at least \(abs(.inputs) - 1), got \(args|length))")
        else
            .
        end
    else if .inputs != (args|length) then
        jqmal_error("Invalid number of arguments (expected \(.inputs), got \(args|length))")
    else
        .
    end end;

def addFrees(newEnv; frees):
    . as $env
    | reduce frees[] as $free (
        $env;
        . as $dot
        | ($free | env_get(newEnv)) as $lookup
        | if $lookup != null then
            env_set_(.; $free; $lookup)
          else
            .
          end)
    | . as $env
    | $env;

def interpret(arguments; env; _eval):
    (if $DEBUG then debug("INTERP: \(. | pr_str(env))") else . end) |
    (select(.kind == "fn") |
        arg_check(arguments) | 
                core_interp(arguments; env) | {expr:., env:env}
    ) //
    (select(.kind == "function") as $fn |
        # todo: arg_check
        (.body | pr_str(env)) as $src |
        # debug("INTERP " + $src) |
        # debug("FREES " + ($fn.free_referencess | tostring)) | 
        .env |
        addFrees(env; $fn.free_referencess) |
        .fallback |= env |
        childEnv($fn.binds; arguments) |
            # tell it about its surroundings
            (reduce $fn.free_referencess[] as $name (
                .;
                . as $env | try env_set(
                    .;
                    $name;
                    $name | env_get(env) // jqmal_error("'\(.)' not found") |
                    . as $xvalue
                    | if $xvalue.kind == "function" then
                        setpath(["free_referencess"]; $fn.free_referencess)
                    else
                        $xvalue
                    end
                ) catch $env)) |
            # tell it about itself
            env_multiset($fn) |
            {
                env: .,
                expr: $fn.body
            }
            | . as $dot
            # | debug("FNEXEC \(.expr | pr_str) \($fn.binds[0] | env_get($dot.env) | pr_str)")
            | _eval 
            | . as $envexp
            |
            {
                expr: .expr,
                env: env
            }
            # | . as $dot
            # | debug("FNPOST \(.expr | pr_str) \($fn.binds[0] | env_get($dot.expr.env) | pr_str)")
            # | debug("INTERP \($src) = \(.expr | pr_str)")
    ) //
        jqmal_error("Unsupported function kind \(.kind)");

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
            |
            if "DEBUG-EVAL" | env_get($_menv) |
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
                                        (
                                        if .env.replEnv != null then
                                            addToEnv($value[1].value)
                                        else
                                            .expr as $def_value |
                                            .env |= env_set_(.; $value[1].value; $def_value)
                                        end
                                        ) as $val |
                                        $val.expr | TCOWrap($val.env; $_orig_retenv; false)
                            ) //
                            (
                                select(.[0].value == "let*") |
                                        (reduce ($value[1].value | nwise(2)) as $xvalue (
                                            # Initial accumulator
                                            {parent:$_menv, environment:{}, fallback:null};
                                            # Loop body
                                            . as $env | $xvalue[1] | EVAL($env) as $expenv |
                                                env_set($expenv.env; $xvalue[0].value; $expenv.expr))) as $env
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
                                select(.[0].value == "if") |
                                    $value[1] | EVAL(env) as $condenv |
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
                                    {
                                        kind: "function",
                                        binds: $binds,
                                        env: $_menv,
                                        body: $value[2],
                                        names: [], # we can't do that circular reference thing
                                        free_referencess: $value[2] | find_free_references($_menv | env_dump_keys + $binds) # for dynamically scoped variables
                                } | TCOWrap($_menv; $_orig_retenv; false)
                            ) //
                            (
                                reduce .[] as $elem (
                                    [];
                                    . as $dot | $elem | EVAL($_menv) as $eval_env |
                                        ($dot + [$eval_env.expr])
                                ) | . as $expr | first |
                                        interpret($expr[1:]; $_menv; _eval_here) as $exprenv |
                                        $exprenv.expr | TCOWrap($exprenv.env; $_orig_retenv; false)
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
                 reduce .[] as $x ({expr:[], env:env};
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
                .value | env_get($_menv) // jqmal_error("'\(.)' not found") |
                TCOWrap($_menv; $_orig_retenv; false)
            ) //
            TCOWrap($_menv; $_orig_retenv; false)
        end
    ) ] |
    last |
    {expr: .ast, env:(.ret_env // .env)};

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

def eval_ign(expr):
    . as $env | expr | READ | EVAL($env) | .env;

# The main program starts here.
    {
        parent: null,
        environment: core_identify,
        fallback: null
    }
    | eval_ign("(def! not (fn* (a) (if a false true)))")
    |
        repl
