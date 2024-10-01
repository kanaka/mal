include "utils";
include "core";
include "env";
include "printer";

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

def extractReplEnv(env):
    env | .replEnv // .;

def extractEnv(env):
    env | .currentEnv // .;

def updateReplEnv(renv):
    def findpath:
        if .env.parent then
            .path += ["parent"] |
            .env |= .parent |
            findpath
        else
            .path
        end;
    ({ env: ., path: [] } | findpath) as $path |
    setpath($path; renv);

def extractCurrentReplEnv(env):
    def findpath:
        if .env.parent then
            .path += ["parent"] |
            .env |= .parent |
            findpath
        else
            .path
        end;
    if env.currentEnv != null then
        ({ env: env.currentEnv, path: [] } | findpath) as $path |
            env.currentEnv | getpath($path)
    else
        env
    end;

def extractAtoms(env):
    env.atoms // {};

def addFrees(newEnv; frees):
    . as $env
    | reduce frees[] as $free (
        $env;
        . as $dot
        | extractEnv(newEnv) as $env
        | ($free | env_get($env)) as $lookup
        | if $lookup != null then
            env_set_(.; $free; $lookup)
          else
            .
          end)
    | . as $env
    | $env;

def interpret(arguments; env; _eval):
    extractReplEnv(env) as $replEnv |
    extractAtoms(env) as $envAtoms |
    (if $DEBUG then debug("INTERP: \(pr_str(env))") end) |
    (select(.kind == "fn") |
        arg_check(arguments) | 
            (select(.function == "eval") | 
                # special function
                { expr: arguments[0], env: $replEnv|wrapEnv($replEnv; $envAtoms) }
                | _eval
                | .env as $xenv
                | extractReplEnv($xenv) as $xreplenv
                | setpath(
                    ["env", "currentEnv"];
                    extractEnv(env) | updateReplEnv($xreplenv))
            ) //
            (select(.function == "reset!") | 
                # env modifying function
                arguments[0].identity as $id |
                ($envAtoms | setpath([$id]; arguments[1])) as $envAtoms |
                arguments[1] |
                {expr:., env: (env | setpath(["atoms"]; $envAtoms))}
            ) //
            (select(.function == "swap!") | 
                # env modifying function
                arguments[0].identity as $id |
                $envAtoms[$id] as $initValue |
                arguments[1] as $function |
                ([$initValue] + arguments[2:]) as $args |
                ($function | interpret($args; env; _eval)) as $newEnvValue |
                ($envAtoms | setpath([$id]; $newEnvValue.expr)) as $envAtoms |
                $newEnvValue.expr |
                {expr:., env:(env | setpath(["atoms"]; $envAtoms))}
            ) // (select(.function == "atom") |
                (now|tostring) as $id |
                {kind: "atom", identity: $id} as $value |
                ($envAtoms | setpath([$id]; arguments[0])) as $envAtoms |
                $value | {expr:., env:(env | setpath(["atoms"]; $envAtoms))}
            ) // (select(.function == "deref") |
                $envAtoms[arguments[0].identity] | {expr:., env:env}
            ) //
            (select(.function  == "apply") |
                # (apply F ...T A) -> (F ...T ...A)
                arguments as $args
                | ($args|first) as $F
                | ($args|last.value) as $A
                | $args[1:-1] as $T
                | $F | interpret([$T[], $A[]]; env; _eval)
            ) //
            (select(.function  == "map") |
                arguments
                | first as $F
                | last.value as $L
                | (reduce $L[] as $elem (
                    {env: env, val: []};
                    . as $dot |
                    ($F | interpret([$elem]; $dot.env; _eval)) as $val |
                    {
                        val: (.val + [$val.expr]),
                        env: (.env | setpath(["atoms"]; $val.env.atoms))
                    }
                  )) as $ex
                | $ex.val | wrap("list") | {expr:., env:$ex.env}
            ) //
                (core_interp(arguments; env) | {expr:., env:env})
    ) //
    (select(.kind == "function") as $fn |
        # todo: arg_check
        (.body | pr_str(env)) as $src |
        # _debug("INTERP " + $src) |
        # _debug("FREES " + ($fn.free_referencess | tostring)) | 
        extractEnv(.env | addFrees(env; $fn.free_referencess)) |
        .fallback |= extractEnv(env) |
        childEnv($fn.binds; arguments) |
            # tell it about its surroundings
            (reduce $fn.free_referencess[] as $name (
                .;
                . as $env | try env_set_(
                    .;
                    $name;
                    $name | env_get(env) // jqmal_error("'\(.)' not found ") |
                    . as $xvalue
                    | if $xvalue.kind == "function" then
                        setpath(["free_referencess"]; $fn.free_referencess)
                    else
                        $xvalue
                    end
                ) catch $env)) |
            # tell it about itself
            env_multiset($fn) |
            wrapEnv($replEnv; $envAtoms) |
            {
                env: .,
                expr: $fn.body
            }
            | . as $dot
            # | debug("FNEXEC \(.expr | pr_str) \($fn.binds[0] | env_get($dot.env) | pr_str)")
            | _eval 
            | . as $envexp
            | (extractReplEnv($envexp.env)) as $xreplenv
            |
            {
                expr: .expr,
                env: extractEnv(env)
                    | updateReplEnv($xreplenv)
                    | wrapEnv($xreplenv; $envexp.env.atoms)
            }
            # | . as $dot
            # | debug("FNPOST \(.expr | pr_str) \($fn.binds[0] | env_get($dot.expr.env) | pr_str)")
            # | debug("INTERP \($src) = \(.expr | pr_str)")
    ) //
        jqmal_error("Unsupported function kind \(.kind)");
        