include "utils";
include "printer";
include "reader";

def core_identify:
    {
        "env": {
            kind: "fn",
            function: "env",
            inputs: 0
        },
        "prn": {
            kind: "fn",
            function: "prn",
            inputs: -1
        },
        "pr-str": {
            kind: "fn",
            function: "pr-str",
            inputs: -1
        },
        "str": {
            kind: "fn",
            function: "str",
            inputs: -1
        },
        "println": {
            kind: "fn",
            function: "println",
            inputs: -1
        },
        "list": {
            kind: "fn",
            function: "list",
            inputs: -1
        },
        "list?": {
            kind: "fn",
            function: "list?",
            inputs: 1
        },
        "empty?": {
            kind: "fn",
            function: "empty?",
            inputs: 1
        },
        "count": {
            kind: "fn",
            function: "count",
            inputs: 1
        },
        "=": {
            kind: "fn",
            function: "=",
            inputs: 2
        },
        "<": {
            kind: "fn",
            function: "<",
            inputs: 2
        },
        "<=": {
            kind: "fn",
            function: "<=",
            inputs: 2
        },
        ">": {
            kind: "fn",
            function: ">",
            inputs: 2
        },
        ">=": {
            kind: "fn",
            function: ">=",
            inputs: 2
        },
        "read-string": {
            kind: "fn",
            function: "read-string",
            inputs: 1
        },
        "slurp": {
            kind: "fn",
            function: "slurp",
            inputs: 1
        },
        "atom": {
            kind: "fn",
            function: "atom",
            inputs: 1
        },
        "atom?": {
            kind: "fn",
            function: "atom?",
            inputs: 1
        },
        "deref": {
            kind: "fn",
            function: "deref",
            inputs: 1
        },
        "reset!": { # defined in interp
            kind: "fn",
            function: "reset!",
            inputs: 2
        },
        "swap!": { # defined in interp
            kind: "fn",
            function: "swap!",
            inputs: -1
        },
        "cons": {
            kind: "fn",
            function: "cons",
            inputs: 2
        },
        "concat": {
            kind: "fn",
            function: "concat",
            inputs: -1
        },
        "nth": {
            kind: "fn",
            function: "nth",
            inputs: 2
        },
        "first": {
            kind: "fn",
            function: "first",
            inputs: 1
        },
        "rest": {
            kind: "fn",
            function: "rest",
            inputs: 1
        }
    };

def vec2list(obj):
    if obj.kind == "list" then
        obj.value | map(vec2list(.)) | wrap("list")
    else 
    if obj.kind == "vector" then
        obj.value | map(vec2list(.)) | wrap("list")
    else 
        obj
    end end;

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
    ) // (
        select(.function == "env") |
        env | tojson | wrap("string")
    ) // (
        select(.function == "prn") |
        arguments | map(pr_str({readable: true})) | join(" ") | _print | null | wrap("nil")
    ) // (
        select(.function == "pr-str") |
        arguments | map(pr_str({readable: true})) | join(" ") |  wrap("string")
    ) // (
        select(.function == "str") |
        arguments | map(pr_str({readable: false})) | join("") |  wrap("string")
    ) // (
        select(.function == "println") |
        arguments | map(pr_str({readable: false})) | join(" ") | _print | null | wrap("nil")
    ) // (
        select(.function == "list") |
        arguments | wrap("list")
    ) // (
        select(.function == "list?") | null | wrap(arguments | first.kind == "list" | tostring)
    ) // (
        select(.function == "empty?") | null | wrap(arguments|first.value | length == 0 | tostring)
    ) // (
        select(.function == "count") | arguments|first.value | length | wrap("number")
    ) // (
        select(.function == "=") | null | wrap(vec2list(arguments[0]) == vec2list(arguments[1]) | tostring)
    ) // (
        select(.function == "<") | null | wrap(arguments[0].value < arguments[1].value | tostring)
    ) // (
        select(.function == "<=") | null | wrap(arguments[0].value <= arguments[1].value | tostring)
    ) // (
        select(.function == ">") | null | wrap(arguments[0].value > arguments[1].value | tostring)
    ) // (
        select(.function == ">=") | null | wrap(arguments[0].value >= arguments[1].value | tostring)
    ) // (
        select(.function == "slurp") | arguments | map(.value) | issue_extern("read") | wrap("string")
    ) // (
        select(.function == "read-string") | arguments | first.value | read_str | read_form.value
    ) // (
        select(.function == "atom") | arguments | first | wrap2("atom"; {names: []})
    ) // (
        select(.function == "atom?") | null | wrap(arguments | first.kind == "atom" | tostring)
    ) // (
        select(.function == "deref") | arguments | first.value
    ) // (
        select(.function == "cons") | ([arguments[0]] + arguments[1].value) | wrap("list")
    ) // (
        select(.function == "concat") | arguments | map(.value) | (add//[]) | wrap("list")
    ) // (
        select(.function == "nth")
            | arguments[0].value as $lst
            | arguments[1].value as $idx
            | if ($lst|length < $idx) or ($idx < 0) then
                jqmal_error("index out of range")
              else
                $lst[$idx]
              end
    ) // (
        select(.function == "first") | arguments[0].value | first // {kind:"nil"}
    ) // (
        select(.function == "rest") | arguments[0]?.value?[1:]? // [] | wrap("list")
    ) // jqmal_error("Unknown native function \(.function)");
