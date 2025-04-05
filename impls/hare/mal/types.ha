use io;
use fmt;
use strings;

export type nil = void;
export type symbol = str;
export type number = int;
export type string = str;
export type list = []MalType;
export type vector = []MalType;
export type collection = (list | vector);
export type intrinsic = *fn(list) (MalType | error);

export type MalValue = (atom | bool | string | hashmap | ...collection | number | symbol | nil);
export type MalType = (...MalValue | function | intrinsic);

export type atom = *MalType;

export type function = struct {
	evaluator:  *fn(MalType, *env) (MalType | error),
	env: *env,
	args: (list | vector),
	body: MalValue,
};


// Any mal object that is supposed to persist, should be created by one of these
// functions. Any allocations done by other functions should be freed manually.
// Hopefully this will make it easier to add garbage collection later.

export fn make_list(s: size, init: list = []) list = {

	if (s == 0) return [];

	let new: list = alloc([nil...], s);
	new[0..len(init)] = init;

	return new;
};

export fn make_vec(s: size, init: list = []) vector = {

	if (s == 0) return [];

	let new: vector = alloc([nil...], s);
	new[0..len(init)] = init;

	return new;
};

export fn make_symbol(name: str) symbol = {
	return strings::dup(name);
};

export fn make_string(s: str) string = {
	return strings::dup(s);
};

export fn make_atom(ref: MalType) atom = {
	return alloc(ref);
};
