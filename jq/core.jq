include "utils";

def core_interp(arguments; env):
    (
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
    ) // jqmal_error("Unknown native function \(.function)");