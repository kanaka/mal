package reader

import (
    "errors"
    "regexp"
    "strconv"
    //"fmt"
)

import (
    "types"
)

type Reader interface {
    next() *string
    peek() *string
}

type TokenReader struct {
    tokens []string
    position int
}

func (tr *TokenReader) next() *string {
    if tr.position >= len(tr.tokens) { return nil }
    token := tr.tokens[tr.position]
    tr.position = tr.position + 1
    return &token
}

func (tr *TokenReader) peek() *string {
    if tr.position > len(tr.tokens) { return nil }
    return &tr.tokens[tr.position]
}



func tokenize (str string) []string {
    results := make([]string, 0, 1)
    re := regexp.MustCompile(`[\s,]*(~@|[\[\]{}()'~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('",;)]*)`)
    for _, group := range re.FindAllStringSubmatch(str, -1) {
        if group[1] == "" { continue }
        results = append(results, group[1])
    }
    return results
}

func read_atom(rdr Reader) (types.MalType, error) {
    token := rdr.next()
    if token == nil { return nil, errors.New("read_atom underflow") }
    if match, _ := regexp.MatchString(`^-?[0-9]+$`, *token); match {
        var i int
        var e error
        if i, e = strconv.Atoi(*token); e != nil {
            return nil, errors.New("number parse error")
        }
        return i, nil
    } else if (*token)[0] == '"' {
        // TODO: unquote newline and quotes
        return (*token)[1:len(*token)-1], nil
    } else if *token == "nil" {
        return nil, nil
    } else if *token == "true" {
        return true, nil
    } else if *token == "false" {
        return false, nil
    } else {
        return types.Symbol{*token}, nil
    }
    return token, nil
}

func read_list(rdr Reader) (types.MalType, error) {
    token := rdr.next()
    if token == nil { return nil, errors.New("read_list underflow") }

    ast_list := []types.MalType{}
    if *token != "(" {
        return nil, errors.New("expected '('")
    }
    token = rdr.peek()
    for ; token != nil && *token != ")" ; token = rdr.peek() {
        if token == nil { return nil, errors.New("exepected ')', got EOF") }
        f, e := read_form(rdr)
        if e != nil { return nil, e }
        ast_list = append(ast_list, f)
    }
    rdr.next()
    return types.List{ast_list}, nil
}

func read_form(rdr Reader) (types.MalType, error) {
    token := rdr.peek()
    if token == nil { return nil, errors.New("read_form underflow") }
    switch (*token) {
    case ")": return nil, errors.New("unexpected ')'")
    case "(": return read_list(rdr)
    default:  return read_atom(rdr)
    }
    return read_atom(rdr)
}

func Read_str(str string) (types.MalType, error) {
    var tokens = tokenize(str);
    if len(tokens) == 0 {
        return nil, errors.New("<empty line>")
    }

    return read_form(&TokenReader{tokens: tokens, position: 0})
}
