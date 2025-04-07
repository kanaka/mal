use fmt;
use os;
use io;
use memio;
use strings;

export fn plus (args: list) (MalType | error) = {

	let result: number = 0;
	for(let n .. args) {
		result += n as number;
	};
	return result;
};

export fn minus (args: list) (MalType | error) = {

	let result: number = args[0] as number;
	for(let n .. args[1..]) {
		result -= n as number;
	};
	return result;
};

export fn mult (args: list) (MalType | error) = {

	let result: number = 1;
	for(let n .. args) {
		result *= n as number;
	};
	return result;
};


export fn div (args: list) (MalType | error) = {
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

export fn mallist (args: list) (MalType | error) = {
	return make_list(len(args), args:list);
};

export fn listp (args: list) (MalType | error) = {
	return args[0] is list;
};

export fn emptyp (args: list) (MalType | error) = {

	let a:[]MalType = match(args[0]){
	case let a: vector =>
		yield a;
	case let a: list =>
		yield a;
	case => return nil;
	};

	return len(a) == 0;
};

export fn count (args: list) (MalType | error) = {
	let arg:[]MalType = match(args[0]) {
	case let a: list =>
		yield a;
	case let a: vector =>
		yield a;
	case nil => return 0;
	};
	return len(arg):number;
};

export fn greater_than (args: list) (MalType | error) = {
	if(len(args) != 2)
		return ("> expected exactly 2 args, got:", args):syntaxError;

	return args[0] as number > args[1] as number;
};

export fn smaller_than (args: list) (MalType | error) = {
	if(len(args) != 2)
		return ("< expected exactly 2 args, got:", args):syntaxError;
	return args[0] as number < args[1] as number;
};


export fn greq_than (args: list) (MalType | error) = {
	if(len(args) != 2)
		return (">= expected exactly 2 args, got:", args):syntaxError;
	return args[0] as number >= args[1] as number;
};

export fn seq_than (args: list) (MalType | error) = {
	if(len(args) != 2)
		return ("<= expected exactly 2 args, got:", args):syntaxError;
	return args[0] as number <= args[1] as number;
};

fn list_cmp (ls: collection, ls2: collection) bool = {

	let x:[]MalType = match(ls){
		case let x:list =>
			yield x;
		case let x:vector =>
			yield x;
	};

	let y:[]MalType = match(ls2){
		case let x:list =>
			yield x;
		case let x:vector =>
			yield x;
	};

	if(!(len(x) == len(y)))
		return false;

	for(let i:size = 0; i < len(x); i += 1){
		if(!(mal_eq([x[i], y[i]]) as bool)){
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
	case let hm:*hm_map =>
		append(q, hm);
	};
	for(let i:size = 0; i < len(q); i += 1){
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
			case let map:*hm_map =>
				append(q, map);
			};
		};
	};

	assert(len(q) == hm1.count);
	return true;
};

export fn mal_eq (args: list) (MalType | error) = {

	match(args[0]){
	case let x: number =>
		if(args[1] is number) {
			return x == args[1] as number;
		};
	case let x: bool =>
		if(args[1] is bool) {
			return x == args[1] as bool;
		};
	case let x: collection =>
		if(args[1] is collection) {
			return list_cmp(x, args[1] as collection);
		};
	case let x: nil =>
		if(args[1] is nil) {
			return true;
		};
	case let x: string =>
		if(args[1] is string) {
			return x == args[1] as string;
		};
	case let s:symbol =>
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

export fn not (args: list) (MalType | error) = {
	match(args[0]){
	case let b: bool =>
		return !b;
	case nil =>
		return true;
	case =>
		return false;
	};
};

export fn prn (args: list) (MalType | error) = {

	for(let i:size = 0; i < len(args); i += 1) {
		print_form(os::stdout, args[i]);
		if (i < len(args) - 1)
			fmt::fprint(os::stdout, " ")!;
	};
	fmt::fprint(os::stdout, "\n")!;
	return nil;
};

export fn prn_line (args: list) (MalType | error) = {

	for(let i:size = 0; i < len(args); i += 1) {
		print_form(os::stdout, args[i], false);
		if (i < len(args) - 1)
			fmt::fprint(os::stdout, " ")!;
	};
	fmt::fprint(os::stdout, "\n")!;
	return nil;
};

export fn pr_str(args: list) (MalType | error) = {

	let strbuf = memio::dynamic();
	defer io::close(&strbuf)!;
	for(let i:size = 0; i < len(args); i += 1) {
		print_form(&strbuf, args[i]);
		if (i < len(args) - 1)
			fmt::fprint(&strbuf, " ")!;
	};

	let s: str = memio::string(&strbuf)!;
	return make_string(s);
};


export fn pr_str_ugly(args: list) (MalType | error) = {

	let strbuf = memio::dynamic();
	defer io::close(&strbuf)!;

	for(let i:size = 0; i < len(args); i += 1) {
		print_form(&strbuf, args[i], false);
	};

	let s: str = memio::string(&strbuf)!;
	return make_string(s);
};

export fn r_string(args: list) (MalType | error) = {

	let input: str = match(args[0]){
	case let s: string =>
		yield s;
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

export fn slurp(args: list) (MalType | error) = {
	let file_name: str = match(args[0]) {
	case let s: string =>
		yield s;
	case =>
		return ("expected string", args[0]):typeerror;
	};

	let file = os::open(file_name)?;
	let fcontent = io::drain(file)?;
	io::close(file)?;

	let s:str = strings::fromutf8(fcontent)!;
	return make_string(s);
};

export fn mal_atom (args: list) (MalType | error) = {
	return make_atom(args[0]);
};

export fn atomp  (args: list) (MalType | error) = {
	return args[0] is atom;
};

export fn atom_deref (args: list) (MalType | error) = {
	match(args[0]){
	case let a: atom =>
		return *a;
	case =>
		return ("expected atom", args[0]):typeerror;
	};
};

export fn atom_reset (args: list) (MalType | error) ={
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

export fn atom_swap (args: list) (MalType | error) = {

	let a: atom = match(args[0]){
	case let a: atom =>
		yield a;
	case =>
		return ("expected atom", args[0]):typeerror;
	};


	let res = match(args[1]){
	case  let func: function =>
		args[0] = func;
		args[1] = *a;
		yield func.evaluator(args, func.env)?;
	case let func: intrinsic =>
		let ls = args[..len(args)-1];
		ls[0] = *a;
		ls[1..] = args[2..];
		yield func(ls)?;
	case =>
		return ("expected function", args[0]):typeerror;
	};

	*a = res;
	return *a;
};

export fn cons(args: list) (MalType | error) = {
	let ls: []MalType = match(args[1]){
	case let ls: list =>
		yield ls;
	case let ls: vector =>
		yield ls;
	case =>
		return("expected list", args[1]):typeerror;
	};

	let new:list = make_list(len(ls)+1);
	new[0] = args[0];
	new[1..] = ls;
	return new;
};

export fn concat(args: list) (MalType | error) = {

	let length:size = 0;
	for(let i:size = 0; i < len(args); i += 1){
		match(args[i]){
		case let ls: list =>
			length += len(ls);
		case let ls: vector =>
			length += len(ls);
		case =>
			return("expected list", args[1]):typeerror;
		};
	};

	if(length == 0) return []:list;

	let new:list = make_list(length);
	let nlen: size = 0;
	for(let i: size = 0; i < len(args); i += 1){
		let ls:[]MalType = match(args[i]){
		case let ls: list =>
			yield ls;
		case let ls: vector =>
			yield ls;
		};

		const n = nlen + len(ls);
		new[nlen..n] = ls;
		nlen = n;
	};
	return new;
};

export fn vec(args: list) (MalType | error) = {
	let ls = match(args[0]){
		case let ls: vector =>
			return ls;
		case let ls: list =>
			yield ls;
		case =>
			return ("expected list or vector",
				args[0]):typeerror;
	};

	let new: vector = [];
	if(len(ls) > 0){
		new = make_list(len(ls));
		new[0..] = ls;
	};

	return new;
};

export fn nth(args: list) (MalType | error) = {
	let ls: []MalType = match(args[0]){
	case let ls:list =>
		yield ls;
	case let ls:vector =>
		yield ls;
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

export fn first(args: list) (MalType | error) = {
	let ls: []MalType = match(args[0]){
	case let ls:list =>
		yield ls;
	case let ls:vector =>
		yield ls;
	case let ls:nil =>
		return nil;
	case =>
		return ("expected list", args):typeerror;
	};

	if(0 == len(ls))
		return nil;

	return ls[0];
};


export fn rest(args: list) (MalType | error) = {
	let ls: []MalType = match(args[0]){
	case let ls:list =>
		yield ls;
	case let ls:vector =>
		yield ls;
	case let ls:nil =>
		return []:list;
	case =>
		return ("expected list", args):typeerror;
	};

	if(0 == len(ls) || 0 == len(ls[1..]))
		return []:list;

	return make_list(len(ls[1..]), ls[1..]);
};

export fn macrop(args: list) (MalType | error) = {
	return args[0] is macro;
};

export fn throw(args: list) (MalType | error) ={
	return ("error", args[0]):malerror;
};


export fn map(args: list) (MalType | error) = {

	let ls:[]MalType = match(args[1]){
	case let l: list =>
		yield l;
	case let l: vector =>
		yield l;
	case =>
		return ("expected list", args):syntaxError;
	};

	const length = len(ls);

	let new = make_list(length);


	for(let i:size = 0; i < len(ls); i += 1){
		new[i] = apply([args[0], [ls[i]]:list]:list)?;
	};

	return new;
};

export fn apply(args: list) (MalType | error) = {

	if(len(args) < 2)
		return ("to few args", args):syntaxError;


	const last = args[len(args)-1];
	const rest = args[1..len(args)-1];


	const last:[]MalType = match(args[len(args)-1]){
	case let l: list =>
		yield l;
	case let l: vector =>
		yield l;
	case =>
		return ("expected list", args):syntaxError;
	};

	let length: size = len(rest) + len(last);

	let ls = make_list(length);
	ls[0 .. len(rest)] = rest;
	ls[len(rest)..] = last;

	match(args[0]){
	case let func: function =>
		let env = env_init(func.env);
		env_bind(env, func.args, ls);
		return func.evaluator(func.body, env);
	case let func: macro =>
		let env = env_init(func.env);
		env_bind(env, func.args, ls);
		return func.evaluator(func.body, env);
	case let func: intrinsic =>
		return func(ls);
	case =>
		return ("expected function", args):syntaxError;
	};
};

export fn nilp(args:list) (MalType | error) = {
	return args[0] is nil;
};

export fn symbolp(args:list) (MalType | error) = {
	match(args[0]){
	case let s: symbol =>
	     if (!(strings::hasprefix(s, ":")))
			return true;
	case => void;
	};
	return false;
};

export fn keywordp(args:list) (MalType | error) = {
	match(args[0]){
	case let s: symbol =>
	     if (strings::hasprefix(s, ":"))
			return true;
	case => void;
	};
	return false;
};

export fn vectorp(args:list) (MalType | error) = {
	return args[0] is vector;
};

export fn sequentialp(args:list) (MalType | error) = {
	return args[0] is collection;
};

export fn mapp(args:list) (MalType | error) = {
	return args[0] is hashmap;
};

export fn truep(args:list) (MalType | error) = {
	match(args[0]){
	case let b:bool =>
		return b;
	case =>
		return false;
	};
};

export fn falsep(args:list) (MalType | error) = {
	match(args[0]){
	case let b:bool =>
		return !b;
	case =>
		return false;
	};
};

export fn malvector(args:list) (MalType | error) = {
	return make_vec(len(args), args);
};

export fn malsymbol(args:list) (MalType | error) = {
	return make_symbol(args[0] as string);
};

export fn malkeyword(args:list) (MalType | error) = {
	match(args[0]){
	case let s: string =>
		let name = strings::lpad(s, ':', len(s) + 1);
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

export fn malhash_map(args:list) (MalType | error) = {
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

fn malhmget(args: list) (MalType | error) = {
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

	return hm_get(hm, key);
};

fn containsp(args: list) (MalType | error) = {
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

fn assoc(args: list) (MalType | error) = {

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

	for(let i:size = 0; i < len(q); i += 1){

		hm_add(new, q[i].key, q[i].val);

		for(let child .. q[i].child){
			match(child){
			case null =>
				continue;
			case let map:*hm_map =>
				append(q, map);
			};
		};
	};

	assert(len(q) == hm.count);
	assert(len(q) == new.count);

	let ls = args[1..];
	for(let i:size = 0; i < len(ls); i += 2){
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

fn dissoc(args: list) (MalType | error) = {

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

	for(let i:size = 0; i < len(q); i += 1){

		let j:size = 0;
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
			case let map:*hm_map =>
				append(q, map);
			};
		};
	};

	return new;
};

fn vals(args: list) (MalType | error) = {

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

	for(let i:size = 0; i < len(q); i += 1){

		new[i] = q[i].val;

		for(let child .. q[i].child){
			match(child){
			case null =>
				continue;
			case let map:*hm_map =>
				append(q, map);
			};
		};
	};

	assert(len(q) == hm.count);
	assert(len(q) == len(new));

	return new;
};

fn keys(args: list) (MalType | error) = {

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

	for(let i:size = 0; i < len(q); i += 1){

		new[i] = q[i].key;

		for(let child .. q[i].child){
			match(child){
			case null =>
				continue;
			case let map:*hm_map =>
				append(q, map);
			};
		};
	};

	assert(len(q) == hm.count);
	assert(len(q) == len(new));

	return new;
};
