package reader

import (
    "errors"
    "regexp"
    "strconv"
    "strings"
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
    if tr.position >= len(tr.tokens) { return nil }
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
        str := (*token)[1:len(*token)-1]
        return strings.Replace(
                 strings.Replace(str, `\"`, `"`, -1),
                 `\n`, "\n", -1), nil
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

func read_list(rdr Reader, start string, end string) (types.MalType, error) {
    token := rdr.next()
    if token == nil { return nil, errors.New("read_list underflow") }

    ast_list := []types.MalType{}
    if *token != start {
        return nil, errors.New("expected '" + start + "'")
    }
    token = rdr.peek()
    for ; true ; token = rdr.peek() {
        if token == nil { return nil, errors.New("exepected '" + end + "', got EOF") }
        if *token == end { break }
        f, e := read_form(rdr)
        if e != nil { return nil, e }
        ast_list = append(ast_list, f)
    }
    rdr.next()
    return types.List{ast_list}, nil
}

func read_vector(rdr Reader) (types.MalType, error) {
    lst, e := read_list(rdr, "[", "]")
    if e != nil { return nil, e }
    vec := types.Vector{lst.(types.List).Val}
    return vec, nil
}

func read_hash_map(rdr Reader) (types.MalType, error) {
    mal_lst, e := read_list(rdr, "{", "}")
    lst := mal_lst.(types.List).Val
    if e != nil { return nil, e }
    if len(lst) % 2 == 1 {
        return nil, errors.New("Odd number of hash map arguments")
    }
    m := map[string]types.MalType{}
    for i := 0; i < len(lst); i+=2 {
        str, ok := lst[i].(string)
        if !ok {
            return nil, errors.New("expected hash-map key string")
        }
        m[str] = lst[i+1]
    }
    return m, nil
}

func read_form(rdr Reader) (types.MalType, error) {
    token := rdr.peek()
    if token == nil { return nil, errors.New("read_form underflow") }
    switch (*token) {
    // list
    case ")": return nil, errors.New("unexpected ')'")
    case "(": return read_list(rdr, "(", ")")

    // vector
    case "]": return nil, errors.New("unexpected ']'")
    case "[": return read_vector(rdr)

    // hash-map
    case "}": return nil, errors.New("unexpected '}'")
    case "{": return read_hash_map(rdr)
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
