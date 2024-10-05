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

def EVAL(env):
    def _eval_here:
        .env as $env | .expr | EVAL($env);

    # EVAL starts here.
            if "DEBUG-EVAL" | env_get(env) |
                . != null and .kind != "false" and .kind != "nil"
            then
                ("EVAL: \(pr_str(env))" | _display | empty), .
            end
            |
            (select(.kind == "list") |
                .value | select(length != 0) as $value |
                            (
                                select(.[0].value == "def!") |
                                    .[1].value as $key |
                                    .[2] | EVAL(env) |
                                    if .env.replEnv != null then
                                        addToEnv($key)
                                    else
                                        .expr as $def_value |
                                        .env |= env_set_(.; $key; $def_value)
                                    end
                            ) //
                            (
                                select(.[0].value == "let*") |
                                        (reduce ($value[1].value | nwise(2)) as $xvalue (
                                            # Initial accumulator
                                            {parent:env, environment:{}, fallback:null};
                                            # Loop body
                                            . as $env | $xvalue[1] | EVAL($env) as $expenv |
                                                env_set($expenv.env; $xvalue[0].value; $expenv.expr))) as $env
                                                    | $value[2] | { expr: EVAL($env).expr, env: env }
                            ) //
                            (
                                select(.[0].value == "do") |
                                    (reduce ($value[1:][]) as $xvalue (
                                        { env: env, expr: {kind:"nil"} };
                                        .env as $env | $xvalue | EVAL($env)
                                    ))
                            ) //
                            (
                                select(.[0].value == "if") |
                                    $value[1] | EVAL(env) as $condenv |
                                        if (["false", "nil"] | contains([$condenv.expr.kind])) then
                                            ($value[3] // {kind:"nil"}) | EVAL($condenv.env)
                                        else
                                            $value[2] | EVAL($condenv.env)
                                        end
                            ) //
                            (
                                select(.[0].value == "fn*") |
                                    # we can't do what the guide says, so we'll skip over this
                                    # and ues the later implementation
                                    # (fn* args body)
                                    $value[1].value | map(.value) as $binds | 
                                    {
                                        kind: "function",
                                        binds: $binds,
                                        env: env,
                                        body: $value[2],
                                        names: [], # we can't do that circular reference thing
                                        free_referencess: $value[2] | find_free_references(env | env_dump_keys + $binds) # for dynamically scoped variables
                                    } | {expr: ., env:env}
                            ) //
                            (
                                reduce .[] as $elem (
                                    [];
                                    . as $dot | $elem | EVAL(env) as $eval_env |
                                        ($dot + [$eval_env.expr])
                                ) | { expr: ., env: env } as $ev
                                    | $ev.expr | first |
                                        interpret($ev.expr[1:]; $ev.env; _eval_here)
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
