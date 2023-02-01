module mal

import regex

const (
	re_token = '^[\\s,]*((?:~@)|(?:[\\[\\]{}()\'\`~^@])|("(?:(?:\\\\.)|[^\\\\"])*"?)|(?:;.*)|(?:[^\\s\\[\\\]{}(\'"`,;)]*))'
	re_int   = '^-?[0-9]+$'
	re_float = '^-?[0-9]*\\.[0-9]+$'
)

type Token = string

struct Reader {
	toks     []Token
	re_int   regex.RE
	re_float regex.RE
mut:
	pos int
}

fn (mut r Reader) next() ?Token {
	if tok := r.peek() {
		r.pos++
		return tok
	}
	return none
}

fn (mut r Reader) peek() ?Token {
	return if r.pos < r.toks.len { r.toks[r.pos] } else { none }
}

fn hash_list(list []MalType) !map[string]MalType {
	mut list_ := list[0..]
	mut hash := map[string]MalType{}
	if list_.len % 2 == 1 {
		return error('extra hashmap param')
	}
	for list_.len > 0 {
		key, val := list_[0], list_[1]
		if key is MalString {
			hash['"${key.val}"'] = val
		} else if key is MalKeyword {
			hash[':${key.key}'] = val
		} else {
			return error('bad hashmap key')
		}
		list_ = list_[2..]
	}
	return hash
}

fn (mut r Reader) macro_wrap(name string, num_forms int) !MalList {
	_ := r.next() or { panic('token underflow') } // consume macro
	mut list := []MalType{}
	for _ in 0 .. num_forms {
		list << r.read_form() or { return error('${name}: missing params') }
	}
	list << MalSymbol{name}
	return MalList{list.reverse()}
}

fn (mut r Reader) read_form() !MalType {
	tok := r.peek() or { return error('no form') }
	return match true {
		tok == '(' { MalList{r.read_list(')')!} }
		tok == '[' { MalVector{r.read_list(']')!} }
		tok == '{' { MalHashmap{hash_list(r.read_list('}')!)!} }
		tok == "'" { r.macro_wrap('quote', 1)! }
		tok == '`' { r.macro_wrap('quasiquote', 1)! }
		tok == '~' { r.macro_wrap('unquote', 1)! }
		tok == '~@' { r.macro_wrap('splice-unquote', 1)! }
		tok == '@' { r.macro_wrap('deref', 1)! }
		tok == '^' { r.macro_wrap('with-meta', 2)! }
		else { r.read_atom()! }
	}
}

fn (mut r Reader) read_list(end_paren string) ![]MalType {
	_ := r.next() or { panic('token underflow') } // consume open paren
	mut list := []MalType{}
	for {
		tok := r.peek() or { return error('unbalanced parens') }
		match true {
			tok == end_paren { break }
			tok in [')', ']', '}'] { return error('unbalanced parens') }
			else { list << r.read_form()! }
		}
	}
	_ := r.next() or { panic('token underflow') } // consume close paren
	return list
}

fn (mut r Reader) read_atom() !MalType {
	tok := r.next() or { panic('token underflow') }
	return match true {
		tok in [')', ']', '}'] { error('unbalanced parens') }
		tok == 'nil' { MalNil{} }
		tok == 'true' { MalTrue{} }
		tok == 'false' { MalFalse{} }
		tok[0] == `"` { MalString{tok[1..tok.len - 1]} }
		tok[0] == `:` { MalKeyword{tok[1..]} }
		r.re_int.matches_string(tok) { MalInt{tok.i64()} }
		r.re_float.matches_string(tok) { MalFloat{tok.f64()} }
		else { MalSymbol{tok} }
	}
}

pub fn read_str(input string) !MalType {
	mut reader := Reader{
		toks: tokenise(input)!
		re_int: regex.regex_opt(mal.re_int) or { panic('regex_opt()') }
		re_float: regex.regex_opt(mal.re_float) or { panic('regex_opt()') }
	}
	return reader.read_form()!
}

fn tokenise(input string) ![]Token {
	mut re := regex.regex_opt(mal.re_token) or { panic('regex_opt()') }
	mut ret := []Token{}
	mut input_ := input
	for {
		$if tokenise ? {
			println('INPUT: [${input_}]')
		}
		start, end := re.match_string(input_)
		if start < 0 {
			break
		}
		$if tokenise ? {
			println('TOKEN: [${input_[re.groups[0]..re.groups[1]]}]')
		}
		if re.groups[1] > re.groups[0] {
			tok := input_[re.groups[0]..re.groups[1]]
			if tok[0] == `"` {
				if tok.len == 1 || tok[tok.len - 1] != `"` {
					return error('unbalanced quotes')
				}
			}
			ret << tok
		}
		input_ = input_[end..]
		if re.groups[1] == re.groups[0] {
			break
		}
	}
	if input_.len > 0 {
		panic('leftover input')
	}
	return ret
}
