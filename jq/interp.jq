include "utils";
include "core";
include "env";
include "printer";

def arg_check(args):
    if .inputs < 0 then
        if (abs(.inputs) - 1) > (args | length) then
            jqmal_error("Invalid number of arguments (expected at least \(abs(.inputs) - 1), got \(args|length): \(args | wrap("vector") | pr_str))")
        else
            .
        end
    else if .inputs != (args|length) then
        jqmal_error("Invalid number of arguments (expected \(.inputs), got \(args|length): \(args | wrap("vector") | pr_str))")
    else
        .
    end end;


def interpret(arguments; env):
    (select(.kind == "fn") |
        arg_check(arguments) | core_interp(arguments; env) 
    ) //
        jqmal_error("Unsupported native function kind \(.kind)");
        

def extractReplEnv(env):
    env | .replEnv // .;

def extractEnv(env):
    env | .currentEnv // .;

def hasReplEnv(env):
    env | has("replEnv");

def cWrapEnv(renv; cond):
    if cond then
        wrapEnv(renv)
    else
        .
    end;

def cUpdateReplEnv(renv; cond):
    def findpath:
        if .env.parent then
            .path += ["parent"] |
            .env |= .parent |
            findpath
        else
            .path
        end;
    if cond then
        ({ env: ., path: [] } | findpath) as $path |
        setpath($path; renv)
    else
        .
    end;

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

def cWithReplEnv(renv; cond):
    if cond then
        extractEnv(.) | wrapEnv(renv)
    else
        .
    end;

def cUpdateAtoms(newEnv; cond):
    . as $env
    | (reduce (extractEnv(newEnv)|.dirty_atoms)[] as $atom (
        $env;
        . as $e | reduce $atom.names[] as $name (
            $e;
            . as $env | env_set_($env; $name; $atom)))) as $resEnv
    | $resEnv | if cond then setpath(["currentEnv", "dirty_atoms"]; []) else . end
    ;

def addFrees(newEnv; frees):
    . as $env
    | reduce frees[] as $free (
        $env;
        . as $dot
        | extractEnv(newEnv) as $env
        | env_req($env; $free) as $lookup
        | if $lookup != null then
            env_set_(.; $free; $lookup)
          else
            .
          end)
    | . as $env
    | $env;

def interpret(arguments; env; _eval):
    extractReplEnv(env) as $replEnv |
    hasReplEnv(env) as $hasReplEnv |
    (select(.kind == "fn") |
        arg_check(arguments) | 
            (select(.function == "eval") | 
                # special function
                { expr: arguments[0], env: $replEnv|cWrapEnv($replEnv; $hasReplEnv) }
                | _eval
                | .env as $xenv
                | extractReplEnv($xenv) as $xreplenv
                | setpath(
                    ["env", "currentEnv"];
                    extractEnv(env) | cUpdateReplEnv($xreplenv; $hasReplEnv))
            ) //
            (select(.function == "reset!") | 
                # env modifying function
                arguments[0].names as $names |
                arguments[1]|wrap2("atom"; {names: $names}) as $value |
                (reduce $names[] as $name (
                    env;
                    . as $env | env_set_($env; $name; $value)
                )) as $env |
                $value.value | addEnv($env | setpath(["currentEnv", "dirty_atoms"]; ($env.currentEnv.dirty_atoms + [$value])|unique))
            ) //
            (select(.function == "swap!") | 
                # env modifying function
                arguments[0].names as $names |
                arguments[0].value as $initValue |
                arguments[1] as $function |
                ([$initValue] + arguments[2:]) as $args |
                ($function | interpret($args; env; _eval)) as $newEnvValue |
                $newEnvValue.expr|wrap2("atom"; {names: $names}) as $newValue |
                $newEnvValue.env as $newEnv |
                (reduce $names[] as $name (
                    $newEnv;
                    . as $env | env_set_($env; $name; $newValue)
                )) as $newEnv |
                $newValue.value | addEnv($newEnv | setpath(["currentEnv", "dirty_atoms"]; ($newEnv.currentEnv.dirty_atoms + [$newValue])|unique))
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
                    [];
                    . + [($F | interpret([$elem]; env; _eval).expr)]
                  )) as $ex
                | $ex | wrap("list") | addEnv(env)
            ) //
                (core_interp(arguments; env) | addEnv(env))
    ) //
    (select(.kind == "function") as $fn |
        # todo: arg_check
        (.body | pr_str) as $src |
        # _debug("INTERP " + $src) |
        # _debug("FREES " + ($fn.free_referencess | tostring)) | 
        env_setfallback(extractEnv(.env | addFrees(env; $fn.free_referencess)); extractEnv(env)) | childEnv($fn.binds; arguments) as $fnEnv |
            # tell it about its surroundings
            (reduce $fn.free_referencess[] as $name (
                $fnEnv;
                . as $env | try env_set_(
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
                env: env_multiset($fnEnv; $fn.names; $fn)
                     | cWrapEnv($replEnv; $hasReplEnv),
                expr: $fn.body
            }
            | . as $dot
            # | _debug("FNEXEC " + (.expr | pr_str) + " " + (env_req($dot.env; $fn.binds[0]) | pr_str))
            | _eval 
            | . as $envexp
            | (extractReplEnv($envexp.env)) as $xreplenv
            |
            {
                expr: .expr,
                env: extractEnv(env)
                    | cUpdateReplEnv($xreplenv; $hasReplEnv)
                    | cWrapEnv($xreplenv; $hasReplEnv)
                    | cUpdateAtoms(extractEnv($envexp.env); $hasReplEnv)
            }
            # | . as $dot
            # | _debug("FNPOST " + (.expr | pr_str) + " " + (env_req($dot.expr.env; $fn.binds[0]) | pr_str))
            # | _debug("INTERP " + $src + " = " + (.expr|pr_str))
    ) //
        jqmal_error("Unsupported function kind \(.kind)");
        