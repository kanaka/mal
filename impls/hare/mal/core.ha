use fmt;
use os;
use io;
use memio;
use strings;
use bufio;
use time;

export type ns_entry = (str, (MalType | *fn([]MalType) (MalType | error)));
export type namespace = []ns_entry;

export fn load_namespace(ns: namespace, env: *env) (void | error) = {
	for(let e.. ns){
		let v: MalType = match(e.1){
		case let v: MalType =>
			yield v;
		case let f: *fn([]MalType) (MalType | error) =>
			yield make_intrinsic(f);
		case =>
			return ("expected MalType", nil):typeerror;
		};
		env_set(env, make_symbol(e.0), v);
	};
};

export let core: namespace = [
	("pr", &prn),
	("list", &mallist),
	("count", &count),
	("list?", &listp),
	("empty?", &emptyp),
	("not", &not),
	("+", &plus),
	("-", &minus),
	("*", &mult),
	("/", &div),
	(">", &greater_than),
	("<", &smaller_than),
	(">=", &greq_than),
	("<=", &seq_than),
	("=", &mal_eq),
	("prn", &prn),
	("println", &prn_line),
	("pr-str", &pr_str),
	("str", &pr_str_ugly),
	("read-string", &r_string),
	("slurp", &slurp),
	("atom", &mal_atom),
	("atom?", &atomp),
	("deref", &atom_deref),
	("reset!", &atom_reset),
	("swap!", &atom_swap),
	("cons", &cons),
	("concat", &concat),
	("vec", &vec),
	("nth", &nth),
	("first", &first),
	("rest", &rest),
	("macro?", &macrop),
	("throw", &throw),
	("apply", &apply),
	("map", &map),
	("nil?", &nilp),
	("true?", &truep),
	("false?", &falsep),
	("symbol?", &symbolp),
	("map?", &mapp),
	("vector", &malvector),
	("vector?", &vectorp),
	("sequential?", &sequentialp),
	("symbol", &malsymbol),
	("keyword?", &keywordp),
	("keyword", &malkeyword),
	("hash-map", &malhash_map),
	("get", &malhmget),
	("contains?", &containsp),
	("assoc", &assoc),
	("dissoc", &dissoc),
	("vals", &vals),
	("keys", &keys),
	("readline", &readline),
	("time-ms", &time_ms),
	("string?", &stringp),
	("number?", &numberp),
	("seq", &seq),
	("conj", &conj),
	("meta", &meta),
	("with-meta", &with_meta),
	("fn?", &fnp),
];

export fn plus (args: []MalType) (MalType | error) = {

	let result: number = 0;
	for(let n .. args) {
		result += n as number;
	};
	return result;
};

export fn minus (args: []MalType) (MalType | error) = {

	let result: number = args[0] as number;
	for(let n .. args[1..]) {
		result -= n as number;
	};
	return result;
};

export fn mult (args: []MalType) (MalType | error) = {

	let result: number = 1;
	for(let n .. args) {
		result *= n as number;
	};
	return result;
};


export fn div (args: []MalType) (MalType | error) = {
	let x = args[0] as number;
	let y = switch(len(args)){
	case 2 =>
	     yield args[1] as number;
	case 1 =>
		yield 1:number;
	case 0 =>
	     yield 1:number;
	case =>
	     yield div(args[1..]) as number;
	};

	return x / y;
};

fn mallist (args: []MalType) (MalType | error) = {
	return make_list(len(args), args);
};

fn listp (args: []MalType) (MalType | error) = {
	return args[0] is list;
};

fn emptyp (args: []MalType) (MalType | error) = {

	let a: []MalType = match(args[0]){
	case let a: vector =>
		yield a.data;
	case let a: list =>
		yield a.data;
	case => return nil;
	};

	return len(a) == 0;
};

fn count (args: []MalType) (MalType | error) = {
	let arg: []MalType = match(args[0]) {
	case let a: list =>
		yield a.data;
	case let a: vector =>
		yield a.data;
	case nil => return 0;
	};
	return len(arg):number;
};

fn greater_than (args: []MalType) (MalType | error) = {
	if(len(args) != 2)
		return ("> expected exactly 2 args, got:", args):syntaxError;

	return args[0] as number > args[1] as number;
};

fn smaller_than (args: []MalType) (MalType | error) = {
	if(len(args) != 2)
		return ("< expected exactly 2 args, got:", args):syntaxError;
	return args[0] as number < args[1] as number;
};


fn greq_than (args: []MalType) (MalType | error) = {
	if(len(args) != 2)
		return (">= expected exactly 2 args, got:", args):syntaxError;
	return args[0] as number >= args[1] as number;
};

fn seq_than (args: []MalType) (MalType | error) = {
	if(len(args) != 2)
		return ("<= expected exactly 2 args, got:", args):syntaxError;
	return args[0] as number <= args[1] as number;
};

fn list_cmp (ls: []MalType, ls2: []MalType) bool = {
	if(!(len(ls) == len(ls2)))
		return false;

	for(let i: size = 0; i < len(ls); i += 1){
		if(!(mal_eq(([ls[i], ls2[i]]:[]MalType)) as bool)){
			return false;
		};
	};
	return true;
};

fn hash_cmp(hm1: hashmap, hm2:hashmap) bool = {

	if(hm1.count != hm2.count) return false;

	let q: []*hm_map= [];
	defer free(q);

	match(*hm1.map){
	case null =>
		if (*hm2.map is null) {
			return true;
		} else {
			return false;
		};
	case let hm: *hm_map =>
		append(q, hm);
	};
	for(let i: size = 0; i < len(q); i += 1){
		match(hm_get(hm2, q[i].key)){
		case nil =>
			return false;
		case let val: MalType =>
			if(!(mal_eq([q[i].val, val]) as bool))
				return false;
		};
		for(let child .. q[i].child){
			match(child){
			case null =>
				continue;
			case let map: *hm_map =>
				append(q, map);
			};
		};
	};

	assert(len(q) == hm1.count);
	return true;
};

fn mal_eq (args: []MalType) (MalType | error) = {

	match(args[0]){
	case let x: number =>
		if(args[1] is number) {
			return x == args[1] as number;
		};
	case let x: bool =>
		if(args[1] is bool) {
			return x == args[1] as bool;
		};

	case let x: list =>
		match(args[1]){
		case let y: vector =>
			return list_cmp(x.data, y.data);
		case let y: list =>
			return list_cmp(x.data, y.data);
		case => void;
		};
	case let x: vector =>
		match(args[1]){
		case let y: vector =>
			return list_cmp(x.data, y.data);
		case let y: list =>
			return list_cmp(x.data, y.data);
		case => void;
		};

	case let x: nil =>
		if(args[1] is nil) {
			return true;
		};
	case let x: string =>
		match(args[1]){
		case let y: string =>
		     return *(x: *str) == *(y: *str);
		case => void;
		};
	case let s: symbol =>
		if(args[1] is symbol){
			return s == args[1] as symbol;
		};
	case let hm: hashmap =>
		if(args[1] is hashmap){
			return hash_cmp(hm, args[1] as hashmap);
		};
	case => void;
	};
	return false;
};

fn not (args: []MalType) (MalType | error) = {
	match(args[0]){
	case let b: bool =>
		return !b;
	case nil =>
		return true;
	case =>
		return false;
	};
};

fn prn (args: []MalType) (MalType | error) = {

	for(let i: size = 0; i < len(args); i += 1) {
		print_form(os::stdout, args[i]);
		if (i < len(args) - 1)
			fmt::fprint(os::stdout, " ")!;
	};
	fmt::fprint(os::stdout, "\n")!;
	return nil;
};

fn prn_line (args: []MalType) (MalType | error) = {

	for(let i: size = 0; i < len(args); i += 1) {
		print_form(os::stdout, args[i], false);
		if (i < len(args) - 1)
			fmt::fprint(os::stdout, " ")!;
	};
	fmt::fprint(os::stdout, "\n")!;
	return nil;
};

fn pr_str(args: []MalType) (MalType | error) = {

	let strbuf = memio::dynamic();
	defer io::close(&strbuf)!;
	for(let i: size = 0; i < len(args); i += 1) {
		print_form(&strbuf, args[i]);
		if (i < len(args) - 1)
			fmt::fprint(&strbuf, " ")!;
	};

	let s: str = memio::string(&strbuf)!;
	return make_string(s);
};


fn pr_str_ugly(args: []MalType) (MalType | error) = {

	let strbuf = memio::dynamic();
	defer io::close(&strbuf)!;

	for(let i: size = 0; i < len(args); i += 1) {
		print_form(&strbuf, args[i], false);
	};

	let s: str = memio::string(&strbuf)!;
	return make_string(s);
};

fn r_string(args: []MalType) (MalType | error) = {

	let input: str = match(args[0]){
	case let s: string =>
		yield *(s: *str);
	case =>
		return ("expected string", args[0]):typeerror;
	};

	match(read_str(strings::toutf8(input))) {
	case io::EOF =>
		return unexpectedEOF;
	case let res: (MalType | error) =>
		return res;
	};
};

fn slurp(args: []MalType) (MalType | error) = {
	let file_name: str = match(args[0]) {
	case let s: string =>
		yield *(s: *str);
	case =>
		return ("expected string", args[0]):typeerror;
	};

	let file = os::open(file_name)?;
	let fcontent = io::drain(file)?;
	io::close(file)?;

	let s: str = strings::fromutf8(fcontent)!;
	return make_string(s);
};

fn mal_atom (args: []MalType) (MalType | error) = {
	return make_atom(args[0]);
};

fn atomp  (args: []MalType) (MalType | error) = {
	return args[0] is atom;
};

fn atom_deref (args: []MalType) (MalType | error) = {
	match(args[0]){
	case let a: atom =>
		return *a;
	case =>
		return ("expected atom", args[0]):typeerror;
	};
};

fn atom_reset (args: []MalType) (MalType | error) ={
	let a: atom = match(args[0]){
	case let a: atom =>
		yield a;
	case =>
		return ("expected atom", args[0]):typeerror;
	};

	let v: MalType = match(args[1]){
	case let v: MalType =>
		yield v;
	case =>
		return ("expected atom", args[0]):typeerror;
	};

	*a = v;
	return v;
};

fn atom_swap (args: []MalType) (MalType | error) = {

	let a: atom = match(args[0]){
	case let a: atom =>
		yield a;
	case =>
		return ("expected atom", args[0]):typeerror;
	};


	let func = match(args[1]){
	case let f: (function | intrinsic) =>
		yield f;
	case =>
		return ("expected function", args[1]):typeerror;
	};

	let appls: list = make_list(len(args[1..]), args[1..]);
	appls.data[0] = *a;

	*a = apply([func, appls])?;

	return *a;
};

fn cons(args: []MalType) (MalType | error) = {

	let ls: []MalType = match(args[1]){

	case let ls: list =>
		yield ls.data;
	case let ls: vector =>
		yield ls.data;
	case =>
		return("expected list", args[1]):typeerror;
	};

	let new: list = make_list(len(ls)+1);
	new.data[0] = args[0];
	new.data[1..] = ls;
	return new;
};

fn concat(args: []MalType) (MalType | error) = {

	let length: size = 0;
	for(let i: size = 0; i < len(args); i += 1){
		match(args[i]){
		case let ls: list =>
			length += len(ls.data);
		case let ls: vector =>
			length += len(ls.data);
		case =>
			return("expected list", args[1]):typeerror;
		};
	};

	if(length == 0) return make_list(0);

	let new: list = make_list(length);
	let nlen: size = 0;
	for(let i: size = 0; i < len(args); i += 1){
		let ls: []MalType = match(args[i]){
		case let ls: list =>
			yield ls.data;
		case let ls: vector =>
			yield ls.data;
		};

		const n = nlen + len(ls);
		new.data[nlen..n] = ls;
		nlen = n;
	};
	return new;
};

fn vec(args: []MalType) (MalType | error) = {

	let ls: []MalType = match(args[0]){
		case let ls: vector =>
			return ls;
		case let ls: list =>
			yield ls.data;
		case =>
			return ("expected list or vector",
				args[0]):typeerror;
	};

	let new: vector = make_vec(len(ls));

	if(len(ls) > 0){
		new.data[0..] = ls;
	};

	return new;
};

fn nth(args: []MalType) (MalType | error) = {
	let ls: []MalType = match(args[0]){
	case let ls: list =>
		yield ls.data;
	case let ls: vector =>
		yield ls.data;
	case =>
		return ("expected list", args):typeerror;
	};

	let index: number = match(args[1]){
	case let i: number =>
		yield i;
	case =>
		return ("expected number", args):typeerror;
	};

	if(index >= len(ls):int)
		return ("bounds error", args):syntaxError;

	return ls[index];
};

fn first(args: []MalType) (MalType | error) = {
	let ls: []MalType = match(args[0]){
	case let ls: list =>
		yield ls.data;
	case let ls: vector =>
		yield ls.data;
	case let ls: nil =>
		return nil;
	case =>
		return ("expected list", args):typeerror;
	};

	if(0 == len(ls))
		return nil;

	return ls[0];
};


fn rest(args: []MalType) (MalType | error) = {
	let ls: []MalType = match(args[0]){
	case let ls: list =>
		yield ls.data;
	case let ls: vector =>
		yield ls.data;
	case let ls: nil =>
		return make_list(0);
	case =>
		return ("expected list", args):typeerror;
	};

	if(0 == len(ls) || 0 == len(ls[1..]))
		return make_list(0);

	return make_list(len(ls[1..]), ls[1..]);
};

fn macrop(args: []MalType) (MalType | error) = {
	return args[0] is macro;
};

fn throw(args: []MalType) (MalType | error) ={
	return ("error", args[0]):malerror;
};


fn map(args: []MalType) (MalType | error) = {

	let ls: []MalType = match(args[1]){
	case let l: list =>
		yield l.data;
	case let l: vector =>
		yield l.data;
	case =>
		return ("expected list", args):syntaxError;
	};

	const length = len(ls);

	let new = make_list(length);


	for(let i: size = 0; i < len(ls); i += 1){

		let argls: []MalType = [ls[i]];
		new.data[i] = apply([args[0], &argls:list])?;
	};

	return new;
};

fn apply(args: []MalType) (MalType | error) = {

	if(len(args) < 2)
		return ("to few args", args):syntaxError;


	const last = args[len(args)-1];
	const rest = args[1..len(args)-1];


	const last:[]MalType = match(args[len(args)-1]){
	case let l: list =>
		yield l.data;
	case let l: vector =>
		yield l.data;
	case =>
		return ("expected list", args):syntaxError;
	};

	let length: size = len(rest) + len(last);

	let ls: []MalType = switch(length){
	case 0 =>
		yield [];
	case =>
		yield alloc([nil...], length);
	};
	defer free(ls);

	ls[0 .. len(rest)] = rest;
	ls[len(rest)..] = last;

	match(args[0]){
	case let func: function =>
		let env = env_init(func.env);
		env_bind(env, func.args, ls);
		return func.eval(func.body, env);
	case let func: macro =>
		let env = env_init(func.env);
		env_bind(env, func.args, ls);
		return func.eval(func.body, env);
	case let f: intrinsic =>
		return f.eval(ls);
	case =>
		return ("expected function", args):syntaxError;
	};
};

fn nilp(args: []MalType) (MalType | error) = {
	return args[0] is nil;
};

fn symbolp(args: []MalType) (MalType | error) = {
	match(args[0]){
	case let s: symbol =>
	     if (!(strings::hasprefix(s, ":")))
			return true;
	case => void;
	};
	return false;
};

fn keywordp(args: []MalType) (MalType | error) = {
	match(args[0]){
	case let s: symbol =>
	     if (strings::hasprefix(s, ":"))
			return true;
	case => void;
	};
	return false;
};

fn vectorp(args: []MalType) (MalType | error) = {
	return args[0] is vector;
};

fn sequentialp(args: []MalType) (MalType | error) = {
	return args[0] is (list | vector);
};

fn mapp(args: []MalType) (MalType | error) = {
	return args[0] is hashmap;
};

fn truep(args: []MalType) (MalType | error) = {
	match(args[0]){
	case let b: bool =>
		return b;
	case =>
		return false;
	};
};

fn falsep(args: []MalType) (MalType | error) = {
	match(args[0]){
	case let b: bool =>
		return !b;
	case =>
		return false;
	};
};

fn malvector(args: []MalType) (MalType | error) = {
	return make_vec(len(args), args);
};

fn malsymbol(args: []MalType) (MalType | error) = {
	let s: str = match(args[0]){
	case let s: string =>
		yield *(s: *str);
	case =>
		return ("expected string", args):typeerror;
	};
	return make_symbol(s);
};

fn malkeyword(args: []MalType) (MalType | error) = {
	match(args[0]){
	case let s: string =>
		let s: *str = s;
		let name = strings::lpad(*s, ':', len(s) + 1);
		defer free(name);
		return make_symbol(name);
	case let k: symbol =>
		if(strings::hasprefix(k, ':'))
			return k;
		return false;
	case =>
		return ("expected string", args):typeerror;
	};
};

fn malhash_map(args: []MalType) (MalType | error) = {
	let new = hm_init();

	if (len(args) % 2 != 0)
		return ("odd number of arguments", args):syntaxError;

	for(let i: size = 0; i < len(args); i += 2){
		match(args[i]){
		case let s: (symbol | string) =>
			hm_add(new, s, args[i+1]);
		case =>
			return ("expected symbol or string",
				args):typeerror;
		};
	};

	return new;
};

fn malhmget(args: []MalType) (MalType | error) = {
	const hm = match(args[0]){
	case let hm: hashmap =>
		yield hm;
	case nil => return nil;
	case =>
		return ("expected hashmap", args):typeerror;
	};

	const key = match(args[1]){
	case let hm: (string | symbol) =>
		yield hm;
	case =>
		return ("expected symbol or string", args):typeerror;
	};

	match (hm_get(hm, key)){
	case let e: undefinedKey =>
		return nil;
	case let v: MalType =>
		return v;
	};
};

fn containsp(args: []MalType) (MalType | error) = {
	const hm = match(args[0]){
	case let hm: hashmap =>
		yield hm;
	case =>
		return ("expected hashmap", args):typeerror;
	};

	const key = match(args[1]){
	case let hm: (string | symbol) =>
		yield hm;
	case =>
		return ("expected symbol or string", args):typeerror;
	};

	match(*hm_find(hm, key)){
	case null => return false;
	case => return true;
	};
};

fn assoc(args: []MalType) (MalType | error) = {

	let hm: hashmap = match(args[0]){
	case let hm: hashmap =>
		yield hm;
	case =>
		return ("expected hashmap", args):typeerror;
	};

	let new: hashmap = hm_init();

	let q: []*hm_map = [];
	defer free(q);

	let map = match(*hm.map){
	case null =>
		void;
	case let map: *hm_map =>
		append(q, map);
	};

	for(let i: size = 0; i < len(q); i += 1){

		hm_add(new, q[i].key, q[i].val);

		for(let child .. q[i].child){
			match(child){
			case null =>
				continue;
			case let map: *hm_map =>
				append(q, map);
			};
		};
	};

	assert(len(q) == hm.count);
	assert(len(q) == new.count);

	let ls = args[1..];
	for(let i: size = 0; i < len(ls); i += 2){
		match(ls[i]){
		case let s: (symbol | string) =>
			hm_set(new, s, ls[i+1]);
		case =>
		     return ("expected symbol or string",
			     args):typeerror;
		};
	};

	return new;
};

fn dissoc(args: []MalType) (MalType | error) = {

	let hm: hashmap = match(args[0]){
	case let hm: hashmap =>
		yield hm;
	case =>
		return ("expected hashmap", args):typeerror;
	};

	let new: hashmap = hm_init();

	let q: []*hm_map = [];
	defer free(q);

	let map = match(*hm.map){
	case null =>
		void;
	case let map: *hm_map =>
		append(q, map);
	};


	let ls = args[1..];

	for(let i: size = 0; i < len(q); i += 1){

		let j: size = 0;
		for(j < len(ls)) {
			let s = match(ls[j]){
			case let s: (symbol | string) =>
				yield s;
			case =>
				return ("expected symbol or string",
					args):typeerror;
			};
			if(keycmp(s, q[i].key)) break;
			j += 1;
		};

		if(len(ls) == j) hm_add(new, q[i].key, q[i].val);

		for(let child .. q[i].child){
			match(child){
			case null =>
				continue;
			case let map: *hm_map =>
				append(q, map);
			};
		};
	};

	return new;
};

fn vals(args: []MalType) (MalType | error) = {

	let hm: hashmap = match(args[0]){
	case let hm: hashmap =>
		yield hm;
	case =>
		return ("expected hashmap", args):typeerror;
	};

	let new: list = make_list(hm.count);

	let q: []*hm_map = [];
	defer free(q);

	let map = match(*hm.map){
	case null =>
		void;
	case let map: *hm_map =>
		append(q, map);
	};

	for(let i: size = 0; i < len(q); i += 1){

		new.data[i] = q[i].val;

		for(let child .. q[i].child){
			match(child){
			case null =>
				continue;
			case let map: *hm_map =>
				append(q, map);
			};
		};
	};

	assert(len(q) == hm.count);
	assert(len(q) == len(new.data));

	return new;
};

fn keys(args: []MalType) (MalType | error) = {

	let hm: hashmap = match(args[0]){
	case let hm: hashmap =>
		yield hm;
	case =>
		return ("expected hashmap", args):typeerror;
	};

	let new: list = make_list(hm.count);

	let q: []*hm_map = [];
	defer free(q);

	let map = match(*hm.map){
	case null =>
		void;
	case let map: *hm_map =>
		append(q, map);
	};

	for(let i: size = 0; i < len(q); i += 1){

		new.data[i] = q[i].key;

		for(let child .. q[i].child){
			match(child){
			case null =>
				continue;
			case let map: *hm_map =>
				append(q, map);
			};
		};
	};

	assert(len(q) == hm.count);
	assert(len(q) == len(new.data));

	return new;
};

fn readline (args: []MalType) (MalType | error) = {
	const prompt:str =  match(args[0]){
	case let p: string =>
		yield *(p: *str);
	case =>
		return ("expected string", args):typeerror;
	};

	fmt::printf(prompt)!;
	bufio::flush(os::stdout)!;

	const input =  match(bufio::read_line(os::stdin)){
	case let input: []u8 =>
		yield input;
	case io::EOF =>
		return nil;
	case let e: io::error =>
		return e;
	};

	const s = strings::fromutf8(input)!;
	const ret = make_string(s);
	free(input);
	return ret;
};

fn time_ms (args: []MalType) (MalType | error) = {
	let now = time::now(time::clock::REALTIME);
	let base = time::instant{sec = 0, ...};
	let diff = time::diff(base, now) / time::MILLISECOND;
	return diff: number;
};

fn stringp (args: []MalType) (MalType | error) = {
	return args[0] is string;
};

fn numberp (args: []MalType) (MalType | error) = {
	return args[0] is number;
};

fn fnp (args: []MalType) (MalType | error) = {
	return args[0] is (function | intrinsic);
};

fn seq (args: []MalType) (MalType | error) = {
	match(args[0]){
	case let s: string =>
		let s: *str = s;
		if(len(s) == 0) return nil;

		let new = make_list(len(*s));

		let it = strings::iter(*s);

		for(let i: size = 0; i < len(*s); i += 1){
			match(strings::next(&it)){
			case let rn: rune =>
				let s: str = strings::fromutf8([rn:u8])!;
				new.data[i] = make_string(s);
			case =>
				break;
			};
		};

		return new;
	case let s: list =>
		if(len(s.data) == 0) return nil;
		return s;
	case let s: vector =>
		if(len(s.data) == 0) return nil;
		return make_list(len(s.data), s.data);
	case let s: nil =>
		return nil;
	};

};

fn conj (args: []MalType) (MalType | error) = {

	let old = args[1..];
	let length = len(old);

	match(args[0]){
	case let ls: list =>
		length += len(ls.data);
		let new = make_list(length);
		new.data[len(old)..] = ls.data;

		for(let i: size = len(old); i > 0; i -= 1){
			new.data[i-1] = old[len(old) - i];
		};
		return new;
	case let ls: vector =>
		length += len(ls.data);
		let new = make_vec(length, ls.data);
		new.data[len(ls.data)..] = old;
		return new;
	case => return ("expected list or vector", args):typeerror;
	};
};

fn meta (args: []MalType) (MalType | error) = {
	match(args[0]){
	case let func: function =>
		return func.meta;
	case let func: intrinsic =>
		return func.meta;
	case let hm: hashmap =>
		return hm.meta;
	case let s: string =>
		return s.meta;
	case let l: list =>
		return l.meta;
	case let v: vector =>
		return v.meta;
	case =>
		return NotImplemented;
	};
};

fn with_meta (args: []MalType) (MalType | error) = {
	match(args[0]){
	case let func: function =>
		let new = make_func(func.eval, func.env, func.args,
				   func.body);
		new.meta = args[1];
		return new;
	case let hm: hashmap =>
		let new = assoc([hm])?:hashmap;
		new.meta = args[1];
		return new;
	case let s: string =>
		let new = make_string(*(s: *str));
		new.meta = args[1];
		return new;
	case let f: intrinsic =>
		let new = make_intrinsic(f.eval);
		new.meta = args[1];
		return new;
	case let ls: list =>
		let new: list = make_list(len(ls.data), ls.data);
		new.meta = args[1];
		return new;
	case let v: vector =>
		let new: vector = make_vec(len(v.data), v.data);
		new.meta = args[1];
		return new;
	case =>
		return NotImplemented;
	};
};
