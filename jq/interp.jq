include "utils";
include "core";
include "env";

def arg_check(args):
    if .inputs == -1 then
        .
    else if .inputs != (args|length) then
        jqmal_error("Invalid number of arguments (expected \(.inputs) got \(args|length): \(args))")
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

def updateAtoms(newEnv):
    . as $env
    | reduce (newEnv | env_dump_keys | map(env_get(newEnv) as $value | select($value.kind == "atom") | $value))[] as $atom (
        $env;
        . as $e | reduce $atom.names[] as $name (
            $e;
            env_set_(.; $name; $atom)));

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
                $value.value | addEnv($env)
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
                $newValue.value | addEnv($newEnv)
            ) //
                (core_interp(arguments; env) | addEnv(env))
    ) //
    (select(.kind == "function") as $fn |
        # todo: arg_check
        env_setfallback(extractEnv(.env); extractEnv(env)) | childEnv($fn.binds; arguments) as $fnEnv |
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
            | _eval 
            | . as $envexp
            | (extractReplEnv($envexp.env)) as $xreplenv
            |
            {
                expr: .expr,
                env: extractEnv(env)
                    | cUpdateReplEnv($xreplenv; $hasReplEnv)
                    | cWrapEnv($xreplenv; $hasReplEnv)
                    | updateAtoms(extractEnv($envexp.env))
            }
    ) //
        jqmal_error("Unsupported function kind \(.kind)");
        