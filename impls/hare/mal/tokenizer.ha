use fmt;
use io;
use strings;
use strconv;
use ascii;

type undefined = !void;

type comment = str;

type list_beg = void;
type vec_beg = void;
type hash_beg = void;
type coll_beg = (list_beg | vec_beg | hash_beg);

type list_end = void;
type vec_end = void;
type hash_end = void;
type coll_end = (list_end | vec_end | hash_end);

type mal_meta = void;

type at = void;
type unquote = void;
type unquote_splice = void;
type quote = void;
type quasiquote = void;
type quote_tk = ( unquote_splice | unquote | quote | quasiquote | at);

type word = str;

type token = (int | str | io::EOF | undefined | ...coll_beg | mal_meta |
	      ...quote_tk | vec_beg | ...coll_end | comment | word);

type tokenizer = struct {
	buffer: []u8,
	un: (token | void),
	rb: (rune | void),
	cursor: size,
	loc: size,
	prev_rn: size,
	prev_t: size,
	next_t: size,
};

fn tokenizer_init(input: []u8) tokenizer = {
	return tokenizer {
		buffer = input,
		un = void,
		rb = void,
		cursor = 0,
		...
	};
};

fn unget_rune(tk: *tokenizer, rn: rune) void = {
	assert(tk.rb is void);
	tk.rb = rn;
	tk.loc = tk.prev_rn;
};

fn unget_token(tk: *tokenizer, tok: token) void = {
	assert(tk.un is void);
	tk.un = tok;
	tk.next_t = tk.loc;
	tk.loc = tk.prev_t;
};

fn nextrune(tk: *tokenizer) (rune | io::EOF) = {

	if(tk.rb is rune){
		const rn = tk.rb as rune;
		tk.rb = void;
		tk.prev_rn = tk.loc;
		tk.loc += 1;
		return rn;
	};

	if (tk.cursor >= len(tk.buffer)) {
		return io::EOF;
	};

	let rn: rune = tk.buffer[tk.cursor]: rune;

	tk.prev_rn = tk.loc;
	tk.loc = tk.cursor;
	tk.cursor += 1;

	return rn;
};

fn iswhitespace(rn: rune) bool = {
	if(ascii::isspace(rn) || rn == ','){
		return true;
	};
	return false;
};

fn nextrunews(tk: *tokenizer) (rune | io::EOF ) = {

	for (true) {
		match (nextrune(tk)) {
		case let rn: rune =>
			if (iswhitespace(rn)) {
				continue;
			};
			return rn;
		case io::EOF =>
			return io::EOF;
		};
	};
};

fn scan_string(tk: *tokenizer) (token | error) = {

	const start = tk.cursor;
	let esc: bool = false;

	for(true){
		const rn = match(nextrune(tk)) {
		case let rn: rune =>
			yield rn;
		case io::EOF =>
			return unexpectedEOF;
		};

		switch(rn){
		case '\\' =>
			esc = !esc;
			continue;
		case '"' =>
			if(esc){
				esc = false;
				continue;
			} else {
				break;
			};
		case =>
		     esc = false;
		     continue;
		};
	};

	return strings::fromutf8(tk.buffer[start .. tk.loc])!;
};

fn scan_comment(tk: *tokenizer) comment = {

	const start = tk.loc;
	let end = start;

	for(true){
		const rn = match(nextrune(tk)){
		case let rn: rune =>
			yield rn;
		case io::EOF =>
			end = tk.cursor;
			break;
		};
		switch(rn){
		case '\n' =>
		     end = tk.loc;
		     break;
		case => continue;
		};

	};

	return (strings::fromutf8(tk.buffer[start .. end])!);

};

fn tokenizer_next(tk: *tokenizer) (token | error) = {

	match(tk.un){
	case let tok: token =>
		tk.un = void;
		tk.prev_t = tk.loc;
		tk.loc = tk.next_t;
		return tok;
	case void =>
		tk.prev_t = tk.loc;
	};

	const rn = match(nextrunews(tk)) {
	case let rn: rune =>
		yield rn;
	case io::EOF =>
		return io::EOF;
	};

	switch (rn) {
	case '(' => return list_beg;
	case ')' => return list_end;
	case '[' => return vec_beg;
	case ']' => return vec_end;
	case '{' => return hash_beg;
	case '}' => return hash_end;
	case '"' => return scan_string(tk);
	case ';' => return scan_comment(tk);
	case '^' => return mal_meta;
	case '\'' => return quote;
	case '`' => return quasiquote;
	case '~' => return scan_quote(tk);
	case '@' => return at;
	case => return scan_atom(tk, rn);
	};
};

fn scan_atom(tk: *tokenizer, rn: rune) (token | error) = {

	if (rn == '-') {
		let nrn = match(nextrune(tk)){
		case io::EOF =>
			yield 'n';
		case let nrn: rune =>
			unget_rune(tk, nrn);
			yield nrn;
		};

		if(ascii::isdigit(nrn)){
			return scan_number(tk);
		};
	} else
	if(ascii::isdigit(rn)){
		return scan_number(tk);
	};

	return scan_word(tk)!;
};

fn scan_number(tk: *tokenizer) (token | error) = {

	const start = tk.loc;
	let end: size = start;

	for(true){
		const rn = match(nextrune(tk)){
		case io::EOF =>
			end = tk.cursor;
			break;
		case let rn: rune =>
			yield rn;
		};

		if(!ascii::isdigit(rn)){
			end = tk.loc;
			unget_rune(tk, rn);
			break;
		};

	};

	return strconv::stoi(strings::fromutf8(tk.buffer[start .. end])!)!;
};

fn scan_word(tk: *tokenizer) (token | error) = {

	const start = tk.loc;
	let end: size = start;

	for(true){
		const rn = match(nextrune(tk)){
		case io::EOF =>
			end = tk.cursor;
			break;
		case let rn: rune =>
			yield rn;
		};

		if(!iswordrn(rn)){
			end = tk.loc;
			unget_rune(tk, rn);
			break;
		};

	};

	return strings::fromutf8(tk.buffer[start .. end])!: word;
};

fn iswordrn(rn: rune) bool = {
	if(ascii::isalnum(rn)){
		return true;
	};

	switch(rn){
	case '-' => return true;
	case '_' => return true;
	case '?' => return true;
	case '!' => return true;
	case '>' => return true;
	case '=' => return true;
	case '<' => return true;
	case '*' => return true;
	case '/' => return true;
	case ':' => return true;
	case => void;
	};

	return false;
};

fn scan_quote(tk: *tokenizer) (token | error) = {
	match(tokenizer_next(tk)?){
	case at =>
	     return unquote_splice;
	case let res: token =>
		unget_token(tk, res);
		return unquote;
	};
};
