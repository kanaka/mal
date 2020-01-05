include "utils";

def arg_check(args):
    if .inputs != (args|length) then
        jqmal_error("Invalid number of arguments (expected \(.inputs) got \(args|length): \(args))")
    else
        .
    end;


def interpret(arguments; env):
    arg_check(arguments) | (
        select(.function == "number_add") |
        arguments | map(.value) | .[0] + .[1] | wrap("number")
    ) // (
        select(.function == "number_sub") |
        arguments | map(.value) | .[0] - .[1] | wrap("number")
    ) // (
        select(.function == "number_mul") |
        arguments | map(.value) | .[0] * .[1] | wrap("number")
    ) // (
        select(.function == "number_div") |
        arguments | map(.value) | .[0] / .[1] | wrap("number")
    ) // jqmal_error("Unknown function \(.function)");