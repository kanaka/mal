use io;

export type nil = void;
export type symbol = str;
export type number = int;
export type atom = (number | symbol);
export type string = str;
export type list = []MalType;
export type vector = []MalType;
export type collection = (list | vector);
export type intrinsic = *fn(list) (MalType | error);
export type MalValue = (bool | string | hashmap | ...collection | ...atom | nil);
export type MalType = (bool | string | function | hashmap | intrinsic | ...collection | ...atom | nil);

export type function = struct {
	env: *env,
	args: (list | vector),
	body: MalValue,
};
