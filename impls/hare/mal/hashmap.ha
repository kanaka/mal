// The hashmap implmentation follows this idea:
// https://nullprogram.com/blog/2023/09/30/

use io;
use fmt;
use hash::fnv;

export type hashmap = *struct {
	data: []hmap,
	meta: MalType,
};

export type hmap = struct {
	key: (symbol | string),
	val: MalType,
	child: [4](size | void),
};

type pos = struct {
	exists: bool,
	index: size,
	child: (size | void),
};

export fn hm_init(gcd: bool = true) hashmap = {
	let new: hashmap = alloc(struct {
		data: []hmap = [],
		meta: MalType = nil,
	});

	if(gcd) append(gc.memory.hashs, new);

	return new;
};

fn hm_free(hm: hashmap) void = {
	free(hm.data);
	free(hm);
};

fn new(
	hm: hashmap,
	p: pos,
	k: (symbol | string),
	v: MalType
) void = {
	const new = hmap {
		key = k,
		val = v,
		child: [4](size | void) = [void...],
	};

	append(hm.data, new);

	match(p.child) {
	case void =>
		return void;
	case let i: size =>
		hm.data[p.index].child[i] = len(hm.data) - 1;
	};

};

export fn keycmp(x: (symbol | string), y: (symbol | string)) bool = {

	const kx: str = match(x){
	case let k: symbol =>
		yield k: str;
	case let k: string =>
		yield k.data;
	};

	const ky: str = match(y){
	case let k: symbol =>
		yield k: str;
	case let k: string =>
		yield k.data;
	};

	return kx == ky;
};

fn hm_find(hm: hashmap, key: (symbol | string)) pos = {

	let index: size = 0;

	const k: str = match(key){
	case let k: symbol =>
		yield k: str;
	case let k: string =>
		yield k.data;
	};

	let hash: u32 = fnv::string32(k);

	if(len(hm.data) == 0)
		return pos {
			exists = false,
			index = 0,
			child = void,
		};

	for(true){
		if (keycmp(key, hm.data[index].key)){
			return pos {
				exists = true,
				index = index,
				child = void,
			};
		};

		let c = hash >> 30;

		match(hm.data[index].child[c]){
		case void =>
			return pos {
				exists = false,
				index = index,
				child = c,
		};
		case let i: size =>
			index = i;
			hash <<= 2;
			continue;
		};
	};
};

export fn hm_set(
	hm: hashmap,
	key: (symbol | string),
	val: MalType,
) void = {

	let p: pos = hm_find(hm, key);

	if(p.exists){
		hm.data[p.index].val = val;
	} else {
		new(hm, p, key, val);
	};

};

export fn hm_add(
	hm: hashmap,
	key: (symbol | string),
	val: MalType,
) void = {

	let p: pos = hm_find(hm, key);

	if(p.exists){
		return void;
	} else {
		new(hm, p, key, val);
	};
};

export fn hm_get(
	hm: hashmap,
	key: (symbol | string)
) (MalType | error) = {

	if(len(hm.data) == 0){
		return ("hm_get 0", key):undefined_key;
	};

	let p: pos = hm_find(hm, key);

	if(p.exists) {
		return hm.data[p.index].val;
	} else  {
		return ("hm_get", key):undefined_key;
	};
};

fn hm_copy(hm: hashmap, filter: [](string | symbol) = []) hashmap = {
	const new = hm_init();

	if(len(filter) == 0){
		for(let e .. hm.data) {
			append(new.data, e);
		};
	} else {
		for :map (let e .. hm.data) {
			for(let f .. filter) {
				if(keycmp(f, e.key))
					continue :map;
			};

			hm_add(new, e.key, e.val);
		};
	};

	return new;
};

fn hm_print(
	strbuf: io::handle,
	hm: hashmap,
	pp: bool,
) void = {
	for (let i: size = 0; i < len(hm.data); i += 1){

		let e = hm.data[i];

		print_form(strbuf, e.key, pp);
		fmt::fprint(strbuf, " ")!;
		print_form(strbuf, e.val, pp);
		if(!(i + 1 == len(hm.data))) fmt::fprint(strbuf, " ")!;
	};
};

fn hash_cmp(hm1: hashmap, hm2: hashmap) bool = {

	if(len(hm1.data) != len(hm2.data)){
		return false;
	};

	for(let i: size = 0; i < len(hm1.data); i += 1) {
		match(hm_get(hm2, hm1.data[i].key)){
		case undefined_key =>
			return false;
		case let v: MalType =>
		     if(!(mal_eq([hm1.data[i].val, v]) as bool))
				return false;
		};
	};

	return true;
};

fn hm_val_list(hm: hashmap) list = {
	const length = len(hm.data);
	const new = make_list(length);

	for(let i: size = 0; i < length; i += 1){
		new.data[i] = hm.data[i].val;
	};

	return new;
};

fn hm_key_list(hm: hashmap) list = {
	const length = len(hm.data);
	const new = make_list(length);

	for(let i: size = 0; i < length; i += 1){
		new.data[i] = hm.data[i].key;
	};
	return new;
};

export fn eval_hash(
	hm: hashmap,
	eval: *fn(MalType, *env) (MalType | error),
	env: *env,
) (hashmap | error) = {

	const new = hm_init();

	for(let e .. hm.data){
		hm_add(new, e.key, eval(e.val, env)?);
	};

	return new;
};
