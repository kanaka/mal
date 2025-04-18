// The hashmap implmentation follows this idea:
// https://nullprogram.com/blog/2023/09/30/

// I decided for this implementation when creating the reader, because of the
// ease of appending, no need for reallocations. In hindsight an array based
// solution would have been a better fit for mal, because immutability means
// lots of copying and that's a pretty heavy operation with this here
// implementation.

use fmt;
use hash::fnv;
use strings;

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

export fn hm_init() hashmap = {

	const new_map = alloc(null: nullable *hm_map);

	const new = alloc(struct {
		meta: MalType = nil,
		count: size = 0,
		map: *nullable *hm_map = new_map,
	});

	append(gc.memory.hashs, new);
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

fn hm_free(hm: hashmap) void = {
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
