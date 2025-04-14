use io;
use fmt;
use memio;
use strings;

export fn read_str(input: []u8) (MalType | error | io::EOF) = {

	const tk: tokenizer = tokenizer_init(input);

	match(read_form(&tk)?){
	case let res: MalType =>
		return res;
	case let e: coll_end =>
		return unbalanced;
	case let res: io::EOF =>
		return io::EOF;
	};

};

fn read_form(tk: *tokenizer) (...MalType | ...coll_end | io::EOF | error) = {
	for(true){
		match(tokenizer_next(tk)?) {
		case let t: coll_beg =>
			return read_collection(tk, t);
		case let t: coll_end =>
			return t;
		case let s: str =>
			return read_string(s);
		case let c: comment => void;
		case let a: word =>
			return read_symbol(a);
		case let q: quote_tk =>
			return read_quote(tk, q);
		case let m: mal_meta =>
			return read_meta(tk);
		case let i: int =>
			return i: number;
		case io::EOF =>
			return io::EOF;
		};
	};
};

fn read_meta(tk: *tokenizer) (list | error) = {

	let res: list = [];
	defer free(res);

	const meta = match(read_form(tk)?){
	case let l: MalType =>
		yield l;
	case coll_end =>
		return unbalanced;
	case io::EOF =>
		return unexpectedEOF;
	};

	const next_form = match(read_form(tk)?){
	case let l: MalType =>
		yield l;
	case coll_end =>
		return unbalanced;
	case io::EOF =>
		return unexpectedEOF;
	};

	return make_list(3, [make_symbol("with-meta"), next_form, meta]);
};

fn read_quote(tk: *tokenizer, t: quote_tk) (list | error) = {

	const qs:symbol = match(t){
	case quote =>
	     yield "quote";
	case unquote =>
	     yield "unquote";
	case quasiquote =>
	     yield "quasiquote";
	case unquote_splice =>
	     yield "splice-unquote";
	case at =>
	     yield "deref";
	};

	const form: MalType = match(read_form(tk)?){
	case let l: MalType =>
		yield l;
	case coll_end =>
		return unbalanced;
	case io::EOF =>
		return unexpectedEOF;
	};

	return make_list(2, [make_symbol(qs), form]);

};

fn read_hashmap(tk: *tokenizer) (hashmap | error) = {

	const res = hm_init();

	for(true){
		let key = match(read_form(tk)?){
		case hash_end =>
			break;
		case let key: (string | symbol) =>
			yield key;
		case io::EOF =>
			return unexpectedEOF;
		};


		let val = match(read_form(tk)?){
		case hash_end =>
			return unbalanced;
		case let form: MalType =>
			yield form;
		case io::EOF =>
			return unexpectedEOF;
		};

		let d = hm_add(res, key, val);
	};

	return res;
};

fn read_collection(tk: *tokenizer, t: coll_beg) (hashmap | ...collection | error) = {

	if(t is hash_beg){
		return read_hashmap(tk);
	};

	let res: []MalType = [];
	defer free(res);

	for(true){
		match(read_form(tk)?){
		case list_end =>
			if(!(t is list_beg)){
				return unbalanced;
			};
			return make_list(len(res), res);
		case vec_end =>
			if(!(t is vec_beg)){
				return unbalanced;
			};
			return make_vec(len(res), res);
		case  hash_end =>
			return unbalanced;
		case let form: MalType =>
			append(res, form);
			continue;
		case io::EOF =>
		     return unexpectedEOF;
		};
	};
};

//todo: keywords as a distinct type
fn read_symbol(s: word) MalType = {
	switch(s){
	case "true" =>
		return true;
	case "false" =>
		return false;
	case "nil" =>
		return nil;
	case =>
		return make_symbol(s);
	};

};

fn read_string(s: str) (string | error) = {

	let strbuf = memio::dynamic();
	defer io::close(&strbuf)!;
	let runes = strings::torunes(s);

	for (let i:size = 0; i < len(runes); i += 1) {
		let rn = switch (runes[i]) {
		case '\\' =>
		     i += 1;
		     yield scan_escape(runes[i]);
		case =>
		     yield runes[i];
		};
		memio::appendrune(&strbuf, rn)!;
	};

	let s: str = memio::string(&strbuf)!;
	return make_string(s);
};

fn scan_escape(rn: rune) rune = {
	switch (rn) {
	case '\"' =>
		return '\"';
	case '\\' =>
		return '\\';
	case 'b' =>
		return '\b';
	case 'f' =>
		return '\f';
	case 'n' =>
		return '\n';
	case 'r' =>
		return '\r';
	case 't' =>
		return '\t';
	case =>
		return rn;
	};
};
