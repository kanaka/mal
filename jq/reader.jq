include "utils";

def tokenize:
    [ . | scan("[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}('\"`,;)]*)") | select(.|length > 0)[0] | select(.[0:1] != ";") ];

def read_str:
    . | tokenize;

# TODO
def read_string:
    . | fromjson;

def extract_string:
    . as $val | if ["keyword", "symbol", "string"] | contains([$val.kind]) then
        $val.value
    else
        error("assoc called with non-string key of type \($val.kind)")
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
                error("EOF while reading string")
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
            empty
            # read_list
        else
            if $lookahead | test("^\\(") then
                [ (.tokens |= .[1:]) | {tokens: .tokens, values: [], finish: false} | (until(.finish;
                    if try (.tokens | first | test("^\\)")) catch true then
                        .finish |= true
                    else 
                        . as $orig | read_form_(depth+1) as $res | {
                            tokens: $res.tokens,
                            values: ($orig.values + [$res.value]),
                            finish: $orig.finish
                        }
                    end)) ] | map(select(.tokens)) | last as $result |
                if $result.tokens | first != ")" then
                    error("unbalanced parentheses in \($result.tokens)")
                else
                    {
                        tokens: $result.tokens[1:],
                        value: {
                            kind: "list",
                            values: $result.values
                        },
                    }
                end
            # read_list '['
            else if $lookahead | test("^\\[") then
                [ (.tokens |= .[1:]) | {tokens: .tokens, values: [], finish: false} | (until(.finish;
                    if try (.tokens | first | test("^\\]")) catch true then
                        .finish |= true
                    else 
                        . as $orig | read_form_(depth+1) as $res | {
                            tokens: $res.tokens,
                            values: ($orig.values + [$res.value]),
                            finish: $orig.finish
                        }
                    end)) ] | map(select(.tokens)) | last as $result |
                if $result.tokens | first != "]" then
                    error("unbalanced brackets in \($result.tokens)")
                else
                    {
                        tokens: $result.tokens[1:],
                        value: {
                            kind: "vector",
                            values: $result.values
                        },
                    }
                end
            # read_list '{'
            else if $lookahead | test("^\\{") then
                [ (.tokens |= .[1:]) | {tokens: .tokens, values: [], finish: false} | (until(.finish;
                    if try (.tokens | first | test("^\\}")) catch true then
                        .finish |= true
                    else 
                        . as $orig | read_form_(depth+1) as $res | {
                            tokens: $res.tokens,
                            values: ($orig.values + [$res.value]),
                            finish: $orig.finish
                        }
                    end)) ] | map(select(.tokens)) | last as $result |
                if $result.tokens | first != "}" then
                    error("unbalanced braces in \($result.tokens)")
                else
                    if $result.values | length % 2 == 1 then
                        # odd number of elements not allowed
                        error("Odd number of parameters to assoc")
                    else
                        {
                            tokens: $result.tokens[1:],
                            value: {
                                kind: "hashmap",
                                values:
                                    [ $result.values | 
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
                        values: [
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
                        values: [
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
                        values: [
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
                        values: [
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
                        values: [
                            {
                                kind: "symbol",
                                value: "deref"
                            },
                            .value
                        ]
                    }
                })
        else
            . as $prev |  read_atom
        end end end end end end end end end);

def read_form:
    {tokens: .} | read_form_(0);
