export type env = struct {
	outer: nullable *env,
	data: hashmap,
};

export fn env_init(outer: nullable * env = null) *env ={
	const new = alloc(env {
		outer = outer,
		data = hm_init(),
	})!;

	append(gc.memory.envs, new)!;
	return new;
};

export fn env_bind(
	env: *env,
	bindings: []MalType,
	exprs: []MalType
) void = {

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
		} else if (bindings[i] as symbol == "&": symbol){
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

export fn env_get(envi: *env, key: symbol) (MalType | undefined_symbol) = {

	match(hm_get(envi.data, key)) {
	case undefined_key =>
		match(envi.outer){
		case null =>
			return ("env_get", key): undefined_symbol;
		case let outer: *env =>
			return env_get(outer, key);
		};
	case let result: MalType =>
		return result;
	};
};
