include "utils";

def tokenize:
    [ . | scan("[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}('\"`,;)]*)") | select(.|length > 0)[0] | select(.[0:1] != ";") ];

def read_str:
    tokenize;

def escape_control:
    (select(. == "\u0000") | "\\u0000") //
    (select(. == "\u0001") | "\\u0001") //
    (select(. == "\u0002") | "\\u0002") //
    (select(. == "\u0003") | "\\u0003") //
    (select(. == "\u0004") | "\\u0004") //
    (select(. == "\u0005") | "\\u0005") //
    (select(. == "\u0006") | "\\u0006") //
    (select(. == "\u0007") | "\\u0007") //
    (select(. == "\u0008") | "\\u0008") //
    (select(. == "\u0009") | "\\u0009") //
    (select(. == "\u0010") | "\\u0010") //
    (select(. == "\u0011") | "\\u0011") //
    (select(. == "\u0012") | "\\u0012") //
    (select(. == "\u0013") | "\\u0013") //
    (select(. == "\u0014") | "\\u0014") //
    (select(. == "\u0015") | "\\u0015") //
    (select(. == "\u0016") | "\\u0016") //
    (select(. == "\u0017") | "\\u0017") //
    (select(. == "\u0018") | "\\u0018") //
    (select(. == "\u0019") | "\\u0019") //
    (select(. == "\u0020") | "\\u0020") //
    (select(. == "\u0021") | "\\u0021") //
    (select(. == "\u0022") | "\\u0022") //
    (select(. == "\u0023") | "\\u0023") //
    (select(. == "\u0024") | "\\u0024") //
    (select(. == "\u0025") | "\\u0025") //
    (select(. == "\u0026") | "\\u0026") //
    (select(. == "\u0027") | "\\u0027") //
    (select(. == "\u0028") | "\\u0028") //
    (select(. == "\u0029") | "\\u0029") //
    (select(. == "\u0030") | "\\u0030") //
    (select(. == "\u0031") | "\\u0031") //
    (select(. == "\n") | "\\n") //
    .;

def read_string:
    gsub("(?<z>[\u0000-\u001f])"; "\(.z | escape_control)") | fromjson;

def extract_string:
    . as $val | if ["keyword", "symbol", "string"] | contains([$val.kind]) then
        $val.value
    else
        jqmal_error("assoc called with non-string key of type \($val.kind)")
    end;

# stuff comes in as {tokens: [...], <stuff unrelated to us>}
def read_atom:
    (.tokens | first) as $lookahead | . | (
        if $lookahead == "nil" then
            {
                tokens: .tokens[1:],
                value: {
                    kind: "nil"
                }
            }
        else if $lookahead == "true" then
            {
                tokens: .tokens[1:],
                value: {
                    kind: "true"
                }
            }
        else if $lookahead == "false" then
            {
                tokens: .tokens[1:],
                value: {
                    kind: "false"
                }
            }
        else if $lookahead | test("^\"") then
            if $lookahead | test("^\"(?:\\\\.|[^\\\\\"])*\"$") then
                {
                    tokens: .tokens[1:],
                    value: {
                        kind: "string",
                        value: $lookahead | read_string
                    }
                }
            else
                jqmal_error("EOF while reading string")
            end
        else if $lookahead | test("^:") then
            {
                tokens: .tokens[1:],
                value: {
                    kind: "keyword",
                    value: $lookahead[1:]
                }
            }
        else if $lookahead | test("^-?[0-9]+(?:\\.[0-9]+)?$") then
            {
                tokens: .tokens[1:],
                value: {
                    kind: "number",
                    value: $lookahead | tonumber
                }
            }
        else if [")", "]", "}"] | contains([$lookahead]) then # this isn't our business
            empty
        else 
            {
                tokens: .tokens[1:],
                value: {
                    kind: "symbol",
                    value: $lookahead
                }
            }
        end end end end end end end
    );

def read_form_(depth):
    (.tokens | first) as $lookahead | . | (
        if $lookahead == null then
            null
            # read_list
        else
            if $lookahead | test("^\\(") then
                [ (.tokens |= .[1:]) | {tokens: .tokens, value: [], finish: false} | (until(.finish;
                    if try (.tokens | first | test("^\\)")) catch true then
                        .finish |= true
                    else 
                        . as $orig | read_form_(depth+1) as $res | {
                            tokens: $res.tokens,
                            value: ($orig.value + [$res.value]),
                            finish: $orig.finish
                        }
                    end)) ] | map(select(.tokens)) | last as $result |
                if $result.tokens | first != ")" then
                    jqmal_error("unbalanced parentheses in \($result.tokens)")
                else
                    {
                        tokens: $result.tokens[1:],
                        value: {
                            kind: "list",
                            value: $result.value
                        },
                    }
                end
            # read_list '['
            else if $lookahead | test("^\\[") then
                [ (.tokens |= .[1:]) | {tokens: .tokens, value: [], finish: false} | (until(.finish;
                    if try (.tokens | first | test("^\\]")) catch true then
                        .finish |= true
                    else 
                        . as $orig | read_form_(depth+1) as $res | {
                            tokens: $res.tokens,
                            value: ($orig.value + [$res.value]),
                            finish: $orig.finish
                        }
                    end)) ] | map(select(.tokens)) | last as $result |
                if $result.tokens | first != "]" then
                    jqmal_error("unbalanced brackets in \($result.tokens)")
                else
                    {
                        tokens: $result.tokens[1:],
                        value: {
                            kind: "vector",
                            value: $result.value
                        },
                    }
                end
            # read_list '{'
            else if $lookahead | test("^\\{") then
                [ (.tokens |= .[1:]) | {tokens: .tokens, value: [], finish: false} | (until(.finish;
                    if try (.tokens | first | test("^\\}")) catch true then
                        .finish |= true
                    else 
                        . as $orig | read_form_(depth+1) as $res | {
                            tokens: $res.tokens,
                            value: ($orig.value + [$res.value]),
                            finish: $orig.finish
                        }
                    end)) ] | map(select(.tokens)) | last as $result |
                if $result.tokens | first != "}" then
                    jqmal_error("unbalanced braces in \($result.tokens)")
                else
                    if $result.value | length % 2 == 1 then
                        # odd number of elements not allowed
                        jqmal_error("Odd number of parameters to assoc")
                    else
                        {
                            tokens: $result.tokens[1:],
                            value: {
                                kind: "hashmap",
                                value:
                                    [ $result.value | 
                                        nwise(2) | 
                                        try {
                                            key: (.[0] | extract_string),
                                            value: {
                                                kkind: .[0].kind,
                                                value: .[1]
                                            }
                                        }
                                    ] | from_entries
                            }
                        }
                    end
                end
        # quote
        else if $lookahead == "'" then
            (.tokens |= .[1:]) | read_form_(depth+1) | (
                {
                    tokens: .tokens,
                    value: {
                        kind: "list",
                        value: [
                            {
                                kind: "symbol",
                                value: "quote"
                            },
                            .value
                        ]
                    }
                })
        # quasiquote
        else if $lookahead == "`" then
            (.tokens |= .[1:]) | read_form_(depth+1) | (
                {
                    tokens: .tokens,
                    value: {
                        kind: "list",
                        value: [
                            {
                                kind: "symbol",
                                value: "quasiquote"
                            },
                            .value
                        ]
                    }
                })
        # unquote
        else if $lookahead == "~" then
            (.tokens |= .[1:]) | read_form_(depth+1) | (
                {
                    tokens: .tokens,
                    value: {
                        kind: "list",
                        value: [
                            {
                                kind: "symbol",
                                value: "unquote"
                            },
                            .value
                        ]
                    }
                })
        # split-unquote
        else if $lookahead == "~@" then
            (.tokens |= .[1:]) | read_form_(depth+1) | (
                {
                    tokens: .tokens,
                    value: {
                        kind: "list",
                        value: [
                            {
                                kind: "symbol",
                                value: "splice-unquote"
                            },
                            .value
                        ]
                    }
                })
        # deref
        else if $lookahead == "@" then
            (.tokens |= .[1:]) | read_form_(depth+1) | (
                {
                    tokens: .tokens,
                    value: {
                        kind: "list",
                        value: [
                            {
                                kind: "symbol",
                                value: "deref"
                            },
                            .value
                        ]
                    }
                })
        # with-meta
        else if $lookahead == "^" then
            (.tokens |= .[1:]) | read_form_(depth+1) as $meta | $meta | read_form_(depth+1) as $value | (
                {
                    tokens: $value.tokens,
                    value: {
                        kind: "list",
                        value: [
                            {
                                kind: "symbol",
                                value: "with-meta"
                            },
                            $value.value,
                            $meta.value
                        ]
                    }
                })
        else
            . as $prev |  read_atom
        end end end end end end end end end end);

def read_form:
    {tokens: .} | read_form_(0);
