package reader

import (
	"errors"
	"regexp"
	"strconv"
	"strings"
	//"fmt"
)

import (
	. "types"
)

type Reader interface {
	next() *string
	peek() *string
}

type TokenReader struct {
	tokens   []string
	position int
}

func (tr *TokenReader) next() *string {
	if tr.position >= len(tr.tokens) {
		return nil
	}
	token := tr.tokens[tr.position]
	tr.position = tr.position + 1
	return &token
}

func (tr *TokenReader) peek() *string {
	if tr.position >= len(tr.tokens) {
		return nil
	}
	return &tr.tokens[tr.position]
}

func tokenize(str string) []string {
	results := make([]string, 0, 1)
	// Work around lack of quoting in backtick
	re := regexp.MustCompile(`[\s,]*(~@|[\[\]{}()'` + "`" +
		`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"` + "`" +
		`,;)]*)`)
	for _, group := range re.FindAllStringSubmatch(str, -1) {
		if (group[1] == "") || (group[1][0] == ';') {
			continue
		}
		results = append(results, group[1])
	}
	return results
}

func read_atom(rdr Reader) (MalType, error) {
	token := rdr.next()
	if token == nil {
		return nil, errors.New("read_atom underflow")
	}
	if match, _ := regexp.MatchString(`^-?[0-9]+$`, *token); match {
		var i int
		var e error
		if i, e = strconv.Atoi(*token); e != nil {
			return nil, errors.New("number parse error")
		}
		return i, nil
	} else if (*token)[0] == '"' {
		str := (*token)[1 : len(*token)-1]
		return strings.Replace(
			strings.Replace(str, `\"`, `"`, -1),
			`\n`, "\n", -1), nil
	} else if (*token)[0] == ':' {
		return NewKeyword((*token)[1:len(*token)])
	} else if *token == "nil" {
		return nil, nil
	} else if *token == "true" {
		return true, nil
	} else if *token == "false" {
		return false, nil
	} else {
		return Symbol{*token}, nil
	}
	return token, nil
}

func read_list(rdr Reader, start string, end string) (MalType, error) {
	token := rdr.next()
	if token == nil {
		return nil, errors.New("read_list underflow")
	}
	if *token != start {
		return nil, errors.New("expected '" + start + "'")
	}

	ast_list := []MalType{}
	token = rdr.peek()
	for ; true; token = rdr.peek() {
		if token == nil {
			return nil, errors.New("exepected '" + end + "', got EOF")
		}
		if *token == end {
			break
		}
		f, e := read_form(rdr)
		if e != nil {
			return nil, e
		}
		ast_list = append(ast_list, f)
	}
	rdr.next()
	return List{ast_list, nil}, nil
}

func read_vector(rdr Reader) (MalType, error) {
	lst, e := read_list(rdr, "[", "]")
	if e != nil {
		return nil, e
	}
	vec := Vector{lst.(List).Val, nil}
	return vec, nil
}

func read_hash_map(rdr Reader) (MalType, error) {
	mal_lst, e := read_list(rdr, "{", "}")
	if e != nil {
		return nil, e
	}
	return NewHashMap(mal_lst)
}

func read_form(rdr Reader) (MalType, error) {
	token := rdr.peek()
	if token == nil {
		return nil, errors.New("read_form underflow")
	}
	switch *token {

	case `'`:
		rdr.next()
		form, e := read_form(rdr)
		if e != nil {
			return nil, e
		}
		return List{[]MalType{Symbol{"quote"}, form}, nil}, nil
	case "`":
		rdr.next()
		form, e := read_form(rdr)
		if e != nil {
			return nil, e
		}
		return List{[]MalType{Symbol{"quasiquote"}, form}, nil}, nil
	case `~`:
		rdr.next()
		form, e := read_form(rdr)
		if e != nil {
			return nil, e
		}
		return List{[]MalType{Symbol{"unquote"}, form}, nil}, nil
	case `~@`:
		rdr.next()
		form, e := read_form(rdr)
		if e != nil {
			return nil, e
		}
		return List{[]MalType{Symbol{"splice-unquote"}, form}, nil}, nil
	case `^`:
		rdr.next()
		meta, e := read_form(rdr)
		if e != nil {
			return nil, e
		}
		form, e := read_form(rdr)
		if e != nil {
			return nil, e
		}
		return List{[]MalType{Symbol{"with-meta"}, form, meta}, nil}, nil
	case `@`:
		rdr.next()
		form, e := read_form(rdr)
		if e != nil {
			return nil, e
		}
		return List{[]MalType{Symbol{"deref"}, form}, nil}, nil

	// list
	case ")":
		return nil, errors.New("unexpected ')'")
	case "(":
		return read_list(rdr, "(", ")")

	// vector
	case "]":
		return nil, errors.New("unexpected ']'")
	case "[":
		return read_vector(rdr)

	// hash-map
	case "}":
		return nil, errors.New("unexpected '}'")
	case "{":
		return read_hash_map(rdr)
	default:
		return read_atom(rdr)
	}
	return read_atom(rdr)
}

func Read_str(str string) (MalType, error) {
	var tokens = tokenize(str)
	if len(tokens) == 0 {
		return nil, errors.New("<empty line>")
	}

	return read_form(&TokenReader{tokens: tokens, position: 0})
}
