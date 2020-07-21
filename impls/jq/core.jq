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
            inputs: -3
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
        "vec": {
            kind: "fn",
            function: "vec",
            inputs: 1
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
        },
        "throw": {
            kind: "fn",
            function: "throw",
            inputs: 1
        },
        "apply": { # defined in interp
            kind: "fn",
            function: "apply",
            inputs: -3
        },
        "map": { # defined in interp
            kind: "fn",
            function: "map",
            inputs: 2
        },
        "nil?": {
            kind: "fn",
            function: "nil?",
            inputs: 1
        },
        "true?": {
            kind: "fn",
            function: "true?",
            inputs: 1
        },
        "false?": {
            kind: "fn",
            function: "false?",
            inputs: 1
        },
        "symbol": {
            kind: "fn",
            function: "symbol",
            inputs: 1
        },
        "symbol?": {
            kind: "fn",
            function: "symbol?",
            inputs: 1
        },
        "keyword": {
            kind: "fn",
            function: "keyword",
            inputs: 1
        },
        "keyword?": {
            kind: "fn",
            function: "keyword?",
            inputs: 1
        },
        "vector": {
            kind: "fn",
            function: "vector",
            inputs: -1
        },
        "vector?": {
            kind: "fn",
            function: "vector?",
            inputs: 1
        },
        "sequential?": {
            kind: "fn",
            function: "sequential?",
            inputs: 1
        },
        "hash-map": {
            kind: "fn",
            function: "hash-map",
            inputs: -1
        },
        "map?": {
            kind: "fn",
            function: "map?",
            inputs: 1
        },
        "assoc": {
            kind: "fn",
            function: "assoc",
            inputs: -2
        },
        "dissoc": {
            kind: "fn",
            function: "dissoc",
            inputs: -2
        },
        "get": {
            kind: "fn",
            function: "get",
            inputs: 2
        },
        "contains?": {
            kind: "fn",
            function: "contains?",
            inputs: 2
        },
        "keys": {
            kind: "fn",
            function: "keys",
            inputs: 1
        },
        "vals": {
            kind: "fn",
            function: "vals",
            inputs: 1
        },
        "string?": {
            kind: "fn",
            function: "string?",
            inputs: 1
        },
        "fn?": {
            kind: "fn",
            function: "fn?",
            inputs: 1
        },
        "number?": {
            kind: "fn",
            function: "number?",
            inputs: 1
        },
        "macro?": {
            kind: "fn",
            function: "macro?",
            inputs: 1
        },
        "readline": {
            kind: "fn",
            function: "readline",
            inputs: 1
        },
        "time-ms": {
            kind: "fn",
            function: "time-ms",
            inputs: 0
        },
        "meta": {
            kind: "fn",
            function: "meta",
            inputs: 1
        },
        "with-meta": {
            kind: "fn",
            function: "with-meta",
            inputs: 2
        },
        "seq": {
            kind: "fn",
            function: "seq",
            inputs: 1
        },
        "conj": {
            kind: "fn",
            function: "conj",
            inputs: -3
        }
    };

def vec2list(obj):
    if obj.kind == "list" then
        obj.value | map(vec2list(.)) | wrap("list")
    else 
        if obj.kind == "vector" then
            obj.value | map(vec2list(.)) | wrap("list")
        else
            if obj.kind == "hashmap" then
                obj.value | map_values(.value |= vec2list(.)) | wrap("hashmap")
            else
                obj
            end
        end
    end;

def make_sequence:
    . as $dot
    | if .value|length == 0 then null | wrap("nil") else
        (
            select(.kind == "string") | .value | split("") | map(wrap("string"))
        ) // (
            select(.kind == "list" or .kind == "vector") | .value
        ) // jqmal_error("cannot make sequence from \(.kind)") | wrap("list")
    end;

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
        arguments | map(pr_str(env; {readable: true})) | join(" ") | _display | null | wrap("nil")
    ) // (
        select(.function == "pr-str") |
        arguments | map(pr_str(env; {readable: true})) | join(" ") |  wrap("string")
    ) // (
        select(.function == "str") |
        arguments | map(pr_str(env; {readable: false})) | join("") |  wrap("string")
    ) // (
        select(.function == "println") |
        arguments | map(pr_str(env; {readable: false})) | join(" ") | _display | null | wrap("nil")
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
        select(.function == "atom?") | null | wrap(arguments | first.kind == "atom" | tostring)
    ) // (
        select(.function == "cons") | ([arguments[0]] + arguments[1].value) | wrap("list")
    ) // (
        select(.function == "concat") | arguments | map(.value) | (add//[]) | wrap("list")
    ) // (
        select(.function == "vec") | {kind:"vector", value:arguments[0].value}
    ) // (
        select(.function == "nth")
            | _debug(arguments)
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
    ) // (
        select(.function == "throw") | jqmal_error(arguments[0] | tojson)
    ) // (
        select(.function == "nil?") | null | wrap((arguments[0].kind == "nil") | tostring)
    ) // (
        select(.function == "true?") | null | wrap((arguments[0].kind == "true") | tostring)
    ) // (
        select(.function == "false?") | null | wrap((arguments[0].kind == "false") | tostring)
    ) // (
        select(.function == "symbol?") | null | wrap((arguments[0].kind == "symbol") | tostring)
    ) // (
        select(.function == "symbol") | arguments[0].value | wrap("symbol")
    ) // (
        select(.function == "keyword") | arguments[0].value | wrap("keyword")
    ) // (
        select(.function == "keyword?") | null | wrap((arguments[0].kind == "keyword") | tostring)
    ) // (
        select(.function == "vector") | arguments | wrap("vector")
    ) // (
        select(.function == "vector?") | null | wrap((arguments[0].kind == "vector") | tostring)
    ) // (
        select(.function == "sequential?") | null | wrap((arguments[0].kind == "vector" or arguments[0].kind == "list") | tostring)
    ) // (
        select(.function == "hash-map") |
            if (arguments|length) % 2 == 1 then
                jqmal_error("Odd number of arguments to hash-map")
            else
                [ arguments | 
                    nwise(2) | 
                    try {
                        key: (.[0] | extract_string),
                        value: {
                            kkind: .[0].kind,
                            value: .[1]
                        }
                    }
                ] | from_entries | wrap("hashmap")
            end
    ) // (
        select(.function == "map?") | null | wrap((arguments[0].kind == "hashmap") | tostring)
    ) // (
        select(.function == "assoc") |
            if (arguments|length) % 2 == 0 then
                jqmal_error("Odd number of key-values to assoc")
            else
                arguments[0].value + ([ arguments[1:] | 
                    nwise(2) | 
                    try {
                        key: (.[0] | extract_string),
                        value: {
                            kkind: .[0].kind,
                            value: .[1]
                        }
                    }
                ] | from_entries) | wrap("hashmap")
            end
    ) // (
        select(.function == "dissoc") | 
            arguments[1:] | map(.value) as $keynames |
            arguments[0].value | with_entries(select(.key as $k | $keynames | contains([$k]) | not)) | wrap("hashmap")
    ) // (
        select(.function == "get") | arguments[0].value[arguments[1].value].value // {kind:"nil"}
    ) // (
        select(.function == "contains?") | null | wrap((arguments[0].value | has(arguments[1].value)) | tostring)
    ) // (
        select(.function == "keys") | arguments[0].value | with_entries(.value as $v | .key as $k | {key: $k, value: {value: $k, kind: $v.kkind}}) | to_entries | map(.value) | wrap("list")
    ) // (
        select(.function == "vals") | arguments[0].value | map(.value) | to_entries | map(.value) | wrap("list")
    ) // (
        select(.function == "string?") | null | wrap((arguments[0].kind == "string") | tostring)
    ) // (
        select(.function == "fn?") | null | wrap((arguments[0].kind == "fn" or (arguments[0].kind == "function" and (arguments[0].is_macro|not))) | tostring)
    ) // (
        select(.function == "number?") | null | wrap((arguments[0].kind == "number") | tostring)
    ) // (
        select(.function == "macro?") | null | wrap((arguments[0].is_macro == true) | tostring)
    ) // (
        select(.function == "readline") | arguments[0].value | __readline | wrap("string")
    ) // (
        select(.function == "time-ms") | now * 1000 | wrap("number")
    ) // (
        select(.function == "meta") | arguments[0].meta // {kind:"nil"}
    ) // (
        select(.function == "with-meta") | arguments[0] | .meta |= arguments[1]
    ) // (
        select(.function == "seq") | arguments[0] | make_sequence
    ) // (
        select(.function == "conj")
            | arguments[0] as $orig
            | arguments[1:] as $stuff
            | if $orig.kind == "list" then
                [ $stuff|reverse[], $orig.value[] ] | wrap("list")
              else
                [ $orig.value[], $stuff[] ] | wrap("vector")
              end
    ) // jqmal_error("Unknown native function \(.function)");
