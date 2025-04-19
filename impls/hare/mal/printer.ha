use io;
use memio;
use fmt;
use strings;

export fn print_form(
	strbuf: io::handle,
	form: (MalType | []MalType),
	print_readably: bool = true
) void = {

	match(form){
	case let l: list =>
		print_list(strbuf, l.data, list_beg, print_readably);
	case let v: vector =>
		print_list(strbuf, v.data, vec_beg, print_readably);
	case let c: []MalType =>
		print_list(strbuf, c, list_beg, print_readably);
	case let h: hashmap =>
		print_hash(strbuf, h, print_readably);
	case let s: string =>
		print_string(strbuf, s, print_readably);
	case nil =>
		memio::concat(strbuf, "nil")!;
	case let b: bool =>
		fmt::fprint(strbuf, b)!;
	case let s: symbol =>
		memio::concat(strbuf, s: str)!;
	case let i: number =>
		fmt::fprint(strbuf, i: int)!;
	case let a: atom =>
		print_list(strbuf, ["atom": symbol, *a],
			list_beg, print_readably);
	case let func: (intrinsic | function) =>
		memio::concat(strbuf, "#<function>")!;
	case => void;
	};
};

fn print_string(
	strbuf: io::handle,
	s: string,
	print_readable: bool
) void = {

	let runes = strings::torunes(s.data);


	if(!print_readable){
		memio::concat(strbuf, s.data)!;
	} else {
		memio::appendrune(strbuf, '"')!;
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
				memio::appendrune(strbuf, rn)!;
			case let rn: str =>
				memio::concat(strbuf, rn)!;
			};
		};
		memio::appendrune(strbuf, '"')!;
	};

};

fn hm_print_rec(
	strbuf: io::handle,
	map: * nullable *hm_map,
	entry: bool,
	pp: bool
) void = {

	match(*map){
	case null => return void;
	case let map: *hm_map =>
		for(let child .. map.child){
			hm_print_rec(strbuf, &child, false, pp);
		};

		print_form(strbuf, map.key, pp);
		fmt::fprint(strbuf, " ")!;
		print_form(strbuf, map.val, pp);

		if(!entry){
			fmt::fprint(strbuf, " ")!;
		};
	};
};


fn print_hash(strbuf: io::handle, hm: hashmap, pp: bool) void ={

	const open = '{';
	const close = '}';

	fmt::fprint(strbuf, open)!;
	hm_print_rec(strbuf, hm.map, true, pp);
	fmt::fprint(strbuf, close)!;
};


fn print_list(
	strbuf: io::handle,
	ls: []MalType,
	t: coll_beg,
	pp: bool
) void = {

	let open = '(';
	let close = ')';

	if(t is vec_beg){
		open = '[';
		close = ']';
	};

	memio::appendrune(strbuf, open)!;
	for(let i: size = 0; i < len(ls); i += 1) {
		let form = print_form(strbuf, ls[i], pp);
		if(!(i == len(ls)-1)){
			fmt::fprint(strbuf, " ")!;
		};
	};
	memio::appendrune(strbuf, close)!;
};
