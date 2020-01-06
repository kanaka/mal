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
    ((select(.kind == "fn") | (
        arg_check(arguments) | core_interp(arguments; env) 
    )) //
        jqmal_error("Unsupported native function kind \(.kind)"));
        
def interpret(arguments; env; _eval):
    (select(.kind == "fn") | (
        arg_check(arguments) | core_interp(arguments; env) | addEnv(env)
    )) //
    (select(.kind == "function") as $fn |
        # todo: arg_check
        .env as $oenv | env_setfallback(.env; env) | childEnv($fn.binds; arguments) as $fnEnv |
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
            { env: env_multiset($fnEnv; $fn.names; $fn), expr: $fn.body } | _eval | { expr: .expr, env: env }
    ) //
        jqmal_error("Unsupported function kind \(.kind)");
