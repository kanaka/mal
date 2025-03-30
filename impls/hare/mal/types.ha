use io;

export type nil = void;
export type symbol = str;
export type number = int;
export type atom = (number | symbol);
export type string = str;
export type list = []MalType;
export type vector = []MalType;
export type collection = (list | vector | hashmap);
export type MalType = (string | ...collection | ...atom | nil);
