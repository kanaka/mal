export type env = struct {
	outer: nullable *env,
	data: hashmap,
};

export fn env_init(outer: nullable * env = null) env ={
	return env {
		outer = outer,
		data = hm_init(),
	};
};



export fn env_set(env: env, key: symbol, val: MalType) void = {
	hm_set(env.data, key, val);
	return void;
};

export fn env_get(envi: env, key: symbol) (MalType | undefinedSymbol) = {

	match(hm_get(envi.data, key)) {
	case nil =>
		match(envi.outer){
		case null =>
			return (void, key):undefinedSymbol;
		case let outer: *env =>
			return env_get(*outer, key);
		};
	case let result: MalType =>
		return result;
	};

	// return hm_get(env.data, key);
};
