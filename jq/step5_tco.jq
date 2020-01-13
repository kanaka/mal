include "reader";
include "printer";
include "utils";
include "core";

def read_line:
    . as $in
    | label $top
    | _readline;

def READ:
    read_str | read_form | .value;

# Environment Functions

def childEnv(binds; exprs):
    {
        parent: .,
        fallback: null,
        environment: [binds, exprs] | transpose | (
            . as $dot | reduce .[] as $item (
                { value: [], seen: false, name: null, idx: 0 };
                if $item[1] != null then
                    if .seen then
                        {
                            value: (.value[1:-1] + (.value|last[1].value += [$item[1]])),
                            seen: true,
                            name: .name
                        }
                    else
                        if $item[0] == "&" then
                            $dot[.idx+1][0] as $name | {
                                value: (.value + [[$name, {kind:"list", value: [$item[1]]}]]),
                                seen: true,
                                name: $name
                            }
                        else
                            {
                                value: (.value + [$item]),
                                seen: false,
                                name: null
                            }
                        end
                    end | (.idx |= .idx + 1)
                else
                    if $item[0] == "&" then
                            $dot[.idx+1][0] as $name | {
                                value: (.value + [[$name, {kind:"list", value: []}]]),
                                seen: true,
                                name: $name
                            }
                    else . end
                end
            )
        ) | .value | map({(.[0]): .[1]}) | add 
    };

def pureChildEnv:
    {
        parent: .,
        environment: {},
        fallback: null
    };

def rootEnv:
    {
        parent: null,
        fallback: null,
        environment: {}
    };

def inform_function(name):
    (.names += [name]) | (.names |= unique);

def inform_function_multi(names):
    . as $dot | reduce names[] as $name(
        $dot;
        inform_function($name)
    );

def env_multiset(keys; value):
    (if value.kind == "function" then # multiset not allowed on atoms
        value | inform_function_multi(keys)
    else
        value
    end) as $value | {
        parent: .parent,
        environment: (
            .environment + (reduce keys[] as $key(.environment; .[$key] |= value))
        ),
        fallback: .fallback
    };

def env_multiset(env; keys; value):
    env | env_multiset(keys; value);

def env_set($key; $value):
    (if $value.kind == "function" or $value.kind == "atom" then
        # inform the function/atom of its names
        ($value |
        if $value.kind == "atom" then
            # check if the one we have is newer
            env_req(env; key) as $ours |
            if $ours.last_modified > $value.last_modified then
                $ours
            else
                # update modification timestamp
                $value | .last_modified |= now
            end
        else
            .
        end) | inform_function($key)
    else 
        $value
    end) as $value | {
        parent: .parent,
        environment: (.environment + (.environment | .[$key] |= $value)), # merge together, as .environment[key] |= value does not work
        fallback: .fallback
    };

def env_dump_keys:
    def _dump1:
        .environment // {} | keys;
    if . == null then [] else
        if .parent == null then
            (
                _dump1 +
                (.fallback | env_dump_keys)
            )
        else
            (
                _dump1 +
                (.parent | env_dump_keys) +
                (.fallback | env_dump_keys)
            )
        end | unique
    end;

def env_find(env):
    if env.environment[.] == null then
        if env.parent then
            env_find(env.parent) // if env.fallback then env_find(env.fallback) else null end
        else
            null
        end
    else
        env
    end;

def env_get(env):
    . as $key | $key | env_find(env).environment[$key] as $value |
    if $value == null then
        jqmal_error("'\($key)' not found")
    else
        if $value.kind == "atom" then
            $value.identity as $id |
            $key | env_find(env.parent).environment[$key] as $possibly_newer |
            if $possibly_newer.identity == $id and $possibly_newer.last_modified > $value.last_modified then
                $possibly_newer
            else
                $value
            end
        else
            $value
        end
    end;

def env_get(env; key):
    key | env_get(env);

def env_req(env; key):
    key as $key | key | env_find(env).environment[$key] as $value |
    if $value == null then
        null
    else
        if $value.kind == "atom" then
            $value.identity as $id |
            $key | env_find(env.parent).environment[$key] as $possibly_newer |
            if $possibly_newer.identity == $id and $possibly_newer.last_modified > $value.last_modified then
                $possibly_newer
            else
                $value
            end
        else
            $value
        end
    end;

def env_set(env; $key; $value):
    (if $value.kind == "function" or $value.kind == "atom" then
        # inform the function/atom of its names
        $value | (.names += [$key]) | (.names |= unique) |
        if $value.kind == "atom" then
            # check if the one we have is newer
            env_req(env; $key) as $ours |
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

def env_setfallback(env; fallback):
    {
        parent: env.parent,
        fallback: fallback,
        environment: env.environment
    };
    
def addEnv(env):
    {
        expr: .,
        env: env
    };

def addToEnv(env; name; expr):
    {
        expr: expr,
        env: env_set(env; name; expr)
    };


def wrapEnv(atoms):
    {
        replEnv: .,
        currentEnv: .,
        atoms: atoms,
        isReplEnv: true
    };

def wrapEnv(replEnv; atoms):
    {
        replEnv: replEnv,
        currentEnv: .,
        atoms: atoms, # id -> value
        isReplEnv: (replEnv == .) # should we allow separate copies?
    };

def unwrapReplEnv:
    .replEnv;

def unwrapCurrentEnv:
    .currentEnv;

def env_set6(env; key; value):
    if env.isReplEnv then
        env_set(env.currentEnv; key; value) | wrapEnv(env.atoms)
    else
        env_set(env.currentEnv; key; value) | wrapEnv(env.replEnv; env.atoms)
    end;

def env_set_(env; key; value):
    if env.currentEnv != null then
        env_set6(env; key; value)
    else
        env_set(env; key; value)
    end;

def addToEnv6(envexp; name):
    envexp.expr as $value
    | envexp.env as $rawEnv
    | (if $rawEnv.isReplEnv then
        env_set_($rawEnv.currentEnv; name; $value) | wrapEnv($rawEnv.atoms)
    else
        env_set_($rawEnv.currentEnv; name; $value) | wrapEnv($rawEnv.replEnv; $rawEnv.atoms)
    end) as $newEnv
    | {
        expr: $value,
        env: $newEnv
    };

def addToEnv(envexp; name):
    if envexp.env.replEnv != null then
        addToEnv6(envexp; name)
    else {
        expr: envexp.expr,
        env: env_set_(envexp.env; name; envexp.expr)
    } end;

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
        | env_req(newEnv; $free) as $lookup
        | if $lookup != null then
            env_set_(.; $free; $lookup)
          else
            .
          end)
    | . as $env
    | $env;

def interpret(arguments; env; _eval):
    (if $DEBUG then _debug("INTERP: \(. | pr_str(env))") else . end) |
    (select(.kind == "fn") |
        arg_check(arguments) | 
                (core_interp(arguments; env) | addEnv(env))
    ) //
    (select(.kind == "function") as $fn |
        # todo: arg_check
        (.body | pr_str(env)) as $src |
        # _debug("INTERP " + $src) |
        # _debug("FREES " + ($fn.free_referencess | tostring)) | 
        env_setfallback((.env | addFrees(env; $fn.free_referencess)); env) | childEnv($fn.binds; arguments) as $fnEnv |
            # tell it about its surroundings
            (reduce $fn.free_referencess[] as $name (
                $fnEnv;
                . as $env | try env_set(
                    .;
                    $name;
                    $name | env_get(env) | . as $xvalue
                    | if $xvalue.kind == "function" then
                        setpath(["free_referencess"]; $fn.free_referencess)
                    else
                        $xvalue
                    end
                ) catch $env)) as $fnEnv |
            # tell it about itself
            env_multiset($fnEnv; $fn.names; $fn) as $fnEnv |
            {
                env: env_multiset($fnEnv; $fn.names; $fn),
                expr: $fn.body
            }
            | . as $dot
            # | _debug("FNEXEC " + (.expr | pr_str) + " " + (env_req($dot.env; $fn.binds[0]) | pr_str))
            | _eval 
            | . as $envexp
            |
            {
                expr: .expr,
                env: env
            }
            # | . as $dot
            # | _debug("FNPOST " + (.expr | pr_str) + " " + (env_req($dot.expr.env; $fn.binds[0]) | pr_str))
            # | _debug("INTERP " + $src + " = " + (.expr|pr_str))
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
                                # (fn* args body)
                                $value[1].value | map(.value) as $binds | {
                                    kind: "function",
                                    binds: $binds,
                                    env: $_menv,
                                    body: $value[2],
                                    names: [], # we can't do that circular reference thing
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
    {stop: false, env: env} | xrepl | if .value then (.value | _display) else empty end;

repl(
    "(def! not (fn* (a) (if a false true)))" | rep(replEnv) | .env
)
