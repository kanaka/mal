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
	eval: *fn(MalType, *env) (MalType | error),
	envi: *env,
	args: []MalType,
	body: MalType,
	meta: MalType,
};

export type macro = function;

export type MalType = (macro | function | intrinsic | atom | bool |
	string | hashmap | list | vector | number | symbol | nil);

// Any mal object that is supposed to persist should be created by one of these
// functions. Any allocations done by other functions should be freed manually.
//
// Envs & Hashmaps are treated separately in their implementation files.

export fn make_intrinsic(
	func: *fn([]MalType) (MalType | error),
) intrinsic = {

	const new = alloc(struct {
		eval: *fn([]MalType) (MalType | error) = func,
		meta: MalType = nil,
	});

	append(gc.memory.intrinsics, new);
	return new;
};

export fn make_func(
	eval: *fn(MalType, *env) (MalType | error),
	envi: *env,
	args: []MalType,
	body: MalType,
) function = {

	let arg_list: []MalType = [];
	if(len(args) > 0) {
		arg_list = alloc([nil...], len(args));
		arg_list[0..] = args;
	};

	const new = alloc(struct{
		eval: *fn(MalType, *env) (MalType | error)  = eval,
		envi: *env= envi,
		args: []MalType = arg_list,
		body: MalType = body,
		meta: MalType = nil });

	append(gc.memory.funcs, new);
	return new;
};

fn free_func(f: function) void = {
	free(f.args);
	free(f);
};

export fn make_list(s: size, init: []MalType = []) list = {

	const new: list = alloc(struct {
		data: []MalType = [],
		meta: MalType = nil,
	});

	if (s == 0) return new;

	new.data = alloc([nil...], s);
	new.data[0..len(init)] = init;

	append(gc.memory.lists, new);
	return new;
};

fn free_list(l: list) void = {
	free(l.data);
	free(l);
};

export fn make_vec(s: size, init: []MalType = []) vector = {

	const new: vector = alloc(struct {
		data: []MalType = [],
		meta: MalType = nil,
	});

	if (s == 0) return new;

	new.data = alloc([nil...], s);
	new.data[0..len(init)] = init;

	append(gc.memory.vecs, new);
	return new;
};

fn free_vec(v: vector) void = {
	free(v.data);
	free(v);
};

export fn make_symbol(name: str) symbol = {

	// this is slow
	for(let s .. gc.memory.symbols){
		if(s == name) return s;
	};

	const new = strings::dup(name): symbol;
	append(gc.memory.symbols, new);

	return new;
};

export fn make_string(s: str) string = {

	const new_str = strings::dup(s);

	const new = alloc(struct {
		data: str = new_str,
		meta: MalType = nil,
	});

	append(gc.memory.strings, new);
	return new;
};

fn free_string(s: string) void = {
	free(s.data);
	free(s);
};

export fn make_atom(ref: MalType) atom = {

	const new = alloc(ref);
	append(gc.memory.atoms, new);
	return new;
};

// check if two strings share the same buffer in memory
// Does not check for substrings!
fn str_memeq(s1: str, s2: str) bool = {

	const ts1 = &s1: *types::string;
	const ts2 = &s2: *types::string;

	return ts1.data == ts2.data;
};
