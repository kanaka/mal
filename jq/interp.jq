include "utils";
include "core";

def arg_check(args):
    if .inputs != (args|length) then
        jqmal_error("Invalid number of arguments (expected \(.inputs) got \(args|length): \(args))")
    else
        .
    end;


def interpret(arguments; env):
    select(.kind == "fn") | (
        arg_check(arguments) | core_interp(arguments; env) 
    ) // jqmal_error("Unsupported function kind \(.kind)");