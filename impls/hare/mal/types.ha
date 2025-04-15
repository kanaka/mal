use io;
use fmt;
use strings;
use types;

export type nil = void;
export type symbol = str;
export type number = i64;
export type atom = *MalType;

export type vector = *struct {
	data: []MalType,
	meta: MalType,
};

export type list = *struct {
	data: []MalType,
	meta: MalType,
};

export type string = *struct {
	data: str,
	meta: MalType,
};

export type intrinsic = *struct {
	eval: *fn([]MalType) (MalType | error),
	meta: MalType,
};

export type function = *struct {
	eval:  *fn(MalType, *env) (MalType | error),
	env: *env,
	args: []MalType,
	body: MalType,
	meta: MalType
};

export type macro = function;

export type MalType = (macro | function | intrinsic | atom | bool |
		       string | hashmap | list | vector | number |
		       symbol | nil);

// Any mal object that is supposed to persist should be created by one of these
// functions. Any allocations done by other functions should be freed manually.
// Hopefully this will make it easier to add garbage collection later.
//
// TODO: Make hashmaps and envs adhere to this.

export fn make_intrinsic(
	func: *fn([]MalType) (MalType | error)
) intrinsic = {

	let new = alloc(struct {
		eval: *fn([]MalType) (MalType | error) = func,
		meta: MalType = nil,
	});
	return new;
};

export fn make_func(
	eval:  *fn(MalType, *env) (MalType | error),
	envi: *env,
	args: []MalType,
	body: MalType
) function = {

       return alloc(struct{
		eval: *fn(MalType, *env) (MalType | error)  = eval,
		env: *env= envi,
		args: []MalType = args,
		body: MalType = body,
		meta: MalType = nil });
};

export fn make_list(s: size, init: []MalType = []) list = {

	let new: list = alloc(struct {
		data: []MalType = [],
		meta: MalType = nil,
	});

	if (s == 0) return new;

	new.data = alloc([nil...], s);
	new.data[0..len(init)] = init;

	return new;
};

export fn make_vec(s: size, init: []MalType = []) vector = {

	let new: vector = alloc(struct {
		data: []MalType = [],
		meta: MalType = nil,
	});

	if (s == 0) return new;

	new.data = alloc([nil...], s);
	new.data[0..len(init)] = init;

	return new;
};

export fn make_symbol(name: str) symbol = {

	static let pool_ptr: nullable *hashmap = null;
	let pool = match(pool_ptr){
	case null =>
		pool_ptr = alloc(hm_init());
		yield *(pool_ptr as *hashmap);
	case let pp: *hashmap =>
		yield *pp;
	};

	match(hm_get(pool, name:symbol)){
	case let s: symbol =>
		return s;
	case undefinedKey => void;
	};
	const new = strings::dup(name):symbol;
	hm_add(pool, new, new);
	return new;
};

export fn make_string(s: str) string = {
	return alloc(struct {
		data:str = strings::dup(s),
		meta:MalType = nil,
		});
};

export fn make_atom(ref: MalType) atom = {
	return alloc(ref);
};

// check if two strings share the same buffer in memory
// Does not check for substrings!
export fn str_memeq(s1: str, s2: str) bool = {

	const ts1 = &s1: *types::string;
	const ts2 = &s2: *types::string;

	return ts1.data == ts2.data;
};
