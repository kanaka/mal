use io;
use memio;
use fmt;
use strings;

export fn print_form(form: MalType)  void = {

	match(form){
	case let l: list =>
		print_list(l, list_beg);
	case let r: vector =>
		print_list(r, vec_beg);
	case let h: hashmap =>
		print_hash(h);
	case let s: string =>
		print_string(s, true);
	case nil =>
		fmt::print("nil")!;
	case let b: bool =>
		fmt::print(b)!;
	case let s: symbol =>
		fmt::print(s)!;
	case let i: number =>
		fmt::print(i: int)!;
	case let func: intrinsic =>
		fmt::print("function")!;
	case => void;
	};
};

fn print_string(s: string, print_readable: bool) void = {

	let strbuf = memio::dynamic();
	let runes = strings::torunes(s);

	if(!print_readable){
		fmt::printf("\"{}\"", s)!;
		return void;
	};

	for(let rn .. runes){
		let ret = switch (rn) {
		case '"' =>
			yield "\\\"";
		case '\\' =>
			yield "\\\\";
		case '\b' =>
			yield "\\b";
		case '\f' =>
			yield "\\f";
		case '\n' =>
			yield "\\n";
		case '\r' =>
			yield "\\r";
		case '\t' =>
			yield "\\t";
		case =>
			yield rn;
		};

		match(ret) {
		case let rn: rune =>
			memio::appendrune(&strbuf, rn)!;
		case let rn: str =>
			memio::concat(&strbuf, rn)!;
		};

	};
	fmt::printf("\"{}\"", memio::string(&strbuf)!)!;
};

fn print_hm(map: *hm_map, i:size) void = {
	print_form(map.key);
	fmt::print(" ")!;
	print_form(map.val);
	if(i != 0){
		fmt::print(" ")!;
	};
};

fn print_hash(hm: hashmap) void ={

	const open = '{';
	const close = '}';

	fmt::print(open)!;
	hm_traverse(hm, &print_hm);
	fmt::print(close)!;
};

fn print_list(ls: []MalType, t: coll_beg) void = {

	let open = '(';
	let close = ')';

	if(t is vec_beg){
		open = '[';
		close = ']';
	};

	fmt::print(open)!;
	for(let i:size = 0; i < len(ls); i += 1) {
		print_form(ls[i]);
		if(!(i == len(ls)-1)){
			fmt::print(" ")!;
		};
	};
	fmt::print(close)!;
};
