// The hashmap implmentation follows this idea:
// https://nullprogram.com/blog/2023/09/30/

// I decided for this implementation when creating the reader, because of the
// ease of appending, no need for reallocations. In hindsight an array based
// solution would have been a better fit for mal, because immutability means
// lots of copying and that's a pretty heavy operation with this here
// implementation.

use hash::fnv;

export type hashmap = *struct {
	meta: MalType,
	count: size,
	map: *nullable *hm_map,
};

export type hm_map = struct {
	key: (symbol | string),
	val: MalType,
	child: [4]nullable *hm_map,
};

fn keycmp(x: (symbol | string), y: (symbol | string)) bool = {

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

fn hm_find(hm: hashmap, key: (symbol | string)) * nullable *hm_map = {

	const k: str = match(key){
	case let k: symbol =>
		yield k: str;
	case let k: string =>
		yield k.data;
	};

	let hash: u32 = fnv::string32(k);
	let map = hm.map;

	for(true){
		let m = match(*map){
		case null =>
			break;
		case let m: *hm_map =>
			yield m;
		};

		if (keycmp(key, m.key)){
			break;
		};

		map = &m.child[hash >> 30];
		hash <<= 2;
	};

	return map;
};

export fn hm_init(gcd: bool = true) hashmap = {

	const new_map = alloc(null: nullable *hm_map);

	const new = alloc(struct {
		meta: MalType = nil,
		count: size = 0,
		map: *nullable *hm_map = new_map,
	});

	if(gcd) append(gc.memory.hashs, new);

	return new;
};

fn hm_new_node(key: (symbol | string), val: MalType) *hm_map = {
	return alloc(hm_map{
			key = key,
			val = val,
			child = [null ...],
	});
};

export fn hm_add(hm: hashmap, key: (symbol | string), elm: MalType) bool = {

	const map = hm_find(hm, key);

	match(*map){
	case null =>
		*map = hm_new_node(key, elm);
		hm.count += 1;
		return true;
	case =>
		return false;
	};

};

export fn hm_set(hm: hashmap, key: (symbol | string), elm: MalType) void = {

	const map = hm_find(hm, key);

	match(*map){
	case null =>
		*map = hm_new_node(key, elm);
		hm.count += 1;
	case let map: *hm_map =>
		map.key = key;
		map.val = elm;
	};
};

export fn hm_get(
	hm: hashmap,
	key: (symbol | string)
) (MalType | undefined_key) = {

	const map = hm_find(hm, key);

	match(*map){
	case null =>
		return ("hm_get", key): undefined_key;
	case let m: *hm_map =>
		return m.val;
	};
};

export fn hm_val_list(hm: hashmap) list = {

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

export fn hm_key_list(hm: hashmap) list = {

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

export fn hm_copy(hm: hashmap, filter: [](string | symbol) = []) hashmap = {
	const new = hm_init();
	hm_copy_rec(hm.map, new, filter);
	return new;
};


fn hm_filterp(key: (symbol | string), filter: [](symbol | string)) bool = {
	for(let f .. filter) {
		if(keycmp(key, f)) return true;

	};
	return false;
};

fn hm_copy_rec(
	old: *nullable *hm_map,
	new: hashmap,
	filter: [](string | symbol)
) void = {

	const map = match(*old){
	case null =>
		return void;
	case let map: *hm_map =>
		yield map;
	};

	if(!hm_filterp(map.key, filter)){
		hm_add(new, map.key, map.val);
	};

	for(let child .. map.child){
		hm_copy_rec(&child, new, filter);
	};
};

export fn hm_free(hm: hashmap) void = {
	hm_free_rec(hm.map);
	free(hm);
};

fn hm_free_rec(map: *nullable *hm_map,) void = {

	match(*map){
	case null =>
		return void;
	case let map: *hm_map =>
		for(let child .. map.child){
			hm_free_rec(&child);
		};
		free(map);
	};
};

export fn eval_hash(
	new: hashmap,
	map: *nullable *hm_map,
	eval: *fn(MalType, *env) (MalType | error),
	env: *env,
) (hashmap | error) = {

	match(*map){
	case null => void;
	case let map: *hm_map =>
		for(let child .. map.child){
			eval_hash(new, &child, eval, env)?;
		};
	hm_add(new, map.key, eval(map.val, env)?);
	};

	return new;
};
