export type env = struct {
	outer: nullable *env,
	data: hashmap,
};

export fn env_init(outer: nullable * env = null) *env ={
	return alloc(env {
		outer = outer,
		data = hm_init(),
	});
};

export fn env_bind(env: *env, bindings: collection, exprs: collection) void = {

	let bindings: []MalType = match(bindings){
	case let b: list =>
		yield b;
	case let b: vector =>
		yield b;
	};

	let exprs: []MalType = match (exprs) {
	case let b: list =>
		yield b;
	case let b: vector =>
		yield b;
	};

	let more: bool = false;
	for(let i: size = 0; i < len(bindings); i += 1){
		if (!(bindings[i] is symbol)){
			return void;
		};
		if (more) {
			let tail = exprs[i - 1..];
			let new = make_list(len(tail), tail);
			env_set(env, bindings[i] as symbol, new);
			break;
		} else if (bindings[i] as symbol == "&":symbol){
			more = true;
			continue;
		} else {
			env_set(env, bindings[i] as symbol, exprs[i]);
		};
	};
};


export fn env_set(env: *env, key: symbol, val: MalType) void = {
	hm_set(env.data, key, val);
	return void;
};

export fn env_get(envi: *env, key: symbol) (MalType | undefinedSymbol) = {

	match(hm_get(envi.data, key)) {
	case nil =>
		match(envi.outer){
		case null =>
			return (void, key):undefinedSymbol;
		case let outer: *env =>
			return env_get(outer, key);
		};
	case let result: MalType =>
		return result;
	};

	// return hm_get(env.data, key);
};
