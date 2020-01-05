def tokenize:
    [ . | scan("[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}('\"`,;)]*)") | select(.|length > 0)[0] | select(.[0:1] != ";") ];

def read_str:
    . | tokenize;

# TODO
def read_string:
    .;

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
                        value: $lookahead[1:-1] | read_string
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
        else if $lookahead == ")" then # this isn't our business
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

def read_form_:
    (.tokens | first) as $lookahead | . | (
        if $lookahead == null then
            empty
        else if $lookahead | test("^\\(") then
            [ . | (.tokens |= .[1:]) | (label $out | foreach .tokens[] as $token (
                {tokens: .tokens, values: []};
                if $token | test("^\\)") then
                    break $out
                else 
                    (. | read_form_) as $res | . | {
                        tokens: $res.tokens,
                        values: (.values + [$res.value])
                    }
                end;
                .)) ] | last as $result |
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
        else
            . | read_atom
        end end);

def read_form:
    {tokens: .} | read_form_;
