use io;
use fmt;

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

