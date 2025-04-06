use hash::fnv;
use fmt;
use strings;

export type hashmap = struct {
	count: size,
	map: * nullable *hm_map,
};

export type hm_map = struct {
	key: (symbol | string),
	val: MalType,
	child: [4]nullable *hm_map,
};

fn keycmp(x: (symbol | string), y: (symbol | string)) bool = {

	const kx: str = match(x){
	case let k: symbol =>
		yield k:str;
	case let k: string =>
		yield k:str;
	};

	const ky: str = match(y){
	case let k: symbol =>
		yield k:str;
	case let k: string =>
		yield k:str;
	};

	return kx == ky;
};

fn hm_find(hm: hashmap, key: (symbol | string)) * nullable *hm_map = {

	let k: str = match(key){
	case let k: symbol =>
		yield k:str;
	case let k: string =>
		yield k:str;
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

export fn hm_init() hashmap = {
	return hashmap {count = 0, map = alloc(null: nullable *hm_map)};
};

fn hm_new_node(key: (symbol | string), val: MalType) *hm_map = {
	return alloc(hm_map{
			key = key,
			val = val,
			child = [null ...],
	});
};

export fn hm_add(hm: hashmap, key: (symbol | string), elm: MalType) bool = {

	let map = hm_find(hm, key);

	match (*map){
	case null =>
		*map = hm_new_node(key, elm);
		hm.count += 1;
		return true;
	case =>
		return false;
	};

};

export fn hm_set(hm: hashmap, key: (symbol | string), elm: MalType) void = {
	let map = hm_find(hm, key);
	match(*map){
	case null =>
		*map = hm_new_node(key, elm);
		hm.count += 1;
	case let map: *hm_map =>
		map.key = key;
		map.val = elm;
	};
};

export fn hm_get(hm: hashmap, key: (symbol | string)) MalType = {

	let map = hm_find(hm, key);
	
	match(*map){
	case null =>
	     return nil;
	case let m:*hm_map =>
	     return m.val;
	};
};

fn hm_traverse(hm: hashmap, act: *fn(*hm_map, size) void) void = {

	hm_traverse_rec(hm.map, hm.count, act);
};

fn hm_traverse_rec(map: * nullable *hm_map, count:size , act: *fn(*hm_map,size) void) void = {

	match(*map){
	case null => return void;
	case let map:*hm_map =>
		for(let child .. map.child){
			hm_traverse_rec(&child, count - 1, act);
		};
		act(map, count);
	};
};
