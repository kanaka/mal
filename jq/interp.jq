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
        jqmal_error("Unsupported native function kind \(.kind)")) | addEnv(env);
        
def interpret(arguments; env; _eval):
    (select(.kind == "fn") | (
        arg_check(arguments) | core_interp(arguments; env) | addEnv(env)
    )) //
    (select(.kind == "function") as $fn |
        # todo: arg_check
        .env as $oenv | .env | childEnv($fn.binds; arguments) as $fnEnv |
            # tell it about its surroundings
            (reduce $fn.corecursives[] as $name (
                $fnEnv;
                env_set(.; $name[0]; $name[1] | setpath(["corecursives"]; $fn.corecursives)))) as $fnEnv |
            # tell it about itself
            env_multiset($fnEnv; $fn.names; $fn) as $fnEnv |
            { env: env_multiset($fnEnv; $fn.names; $fn), expr: $fn.body } | _eval | { expr: .expr, env: env }
    ) //
        jqmal_error("Unsupported function kind \(.kind)");

