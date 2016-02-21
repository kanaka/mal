function env_new(outer, params, args,    idx, len, i, j, lst, param)
{
	if (params != "") {
		params = substr(params, 2)
		len = types_heap[params]["len"]
		if (len >= 2 && types_heap[params][len - 2] == "'&") {
			if (types_heap[args]["len"] < len - 1) {
				return "!\"Invalid argument length for the function. Expects at least " (len - 2) " arguments, supplied " (types_heap[args]["len"] - 1) "."
			}
		} else {
			if (types_heap[args]["len"] != len + 1) {
				return "!\"Invalid argument length for the function. Expects exactly " len " arguments, supplied " (types_heap[args]["len"] - 1) "."
			}
		}
	}
	env_heap[env_heap_index]["ref"] = 1
	env_heap[env_heap_index]["outer"] = outer
	if (params != "") {
		for (i = 0; i < len; ++i) {
			param = types_heap[params][i]
			if (param == "'&") {
				idx = types_allocate()
				env_set(env_heap_index, types_heap[params][++i], "(" idx)
				len = types_heap[args]["len"]
				for (j = 0; i < len; ++j) {
					types_addref(types_heap[idx][j] = types_heap[args][i++])
				}
				types_heap[idx]["len"] = j
				break
			}
			env_set(env_heap_index, param, types_heap[args][i + 1])
			types_addref(types_heap[args][i + 1])
		}
	}
	if (outer != "") {
		env_addref(outer)
	}
	return env_heap_index++
}

function env_set(env, key, val)
{
	if (key in env_heap[env]) {
		types_release(env_heap[env][key])
	}
	if (val ~ /^\&/) {
		env_builtinnames[substr(val, 2)] = substr(key, 2)
	}
	env_heap[env][key] = val
}

function env_find(env, key)
{
	while (env != "") {
		if (key in env_heap[env]) {
			return env
		}
		env = env_heap[env]["outer"]
	}
	return env
}

function env_get(env, key)
{
	env = env_find(env, key)
	if (env != "") {
		return env_heap[env][key]
	}
	return "!\"'" substr(key, 2) "' not found"
}

function env_addref(env)
{
	env_heap[env]["ref"]++
}

function env_release(env,    i, outer)
{
	while (env != "" && --env_heap[env]["ref"] == 0) {
		for (i in env_heap[env]) {
			if (i ~ /^'/) {
				types_release(env_heap[env][i])
			}
		}
		outer = env_heap[env]["outer"]
		delete env_heap[env]
		env = outer
	}
}

function env_dump(i, j)
{
	for (i = 0; i < env_heap_index; i++) {
		if (i in env_heap) {
			if (isarray(env_heap[i])) {
				if (!("checked" in env_heap[i]) || env_heap[i]["checked"] != env_heap[i]["ref"]) {
					for (j in env_heap[i]) {
						print "  env_heap[" i "][" j "] = " env_heap[i][j]
					}
				}
			} else {
				print "  env_heap[" i "] = " env_heap[i]
			}
		}
	}
}

function env_check(env,    i, outer)
{
	if (env_heap[env]["checked"]++) {
		return
	}
	for (i in env_heap[env]) {
		if (i != "ref" && i != "outer") {
			types_check(env_heap[env][i])
		}
	}
	outer = env_heap[env]["outer"]
	if (outer in env_heap) {
		env_check(outer)
	}
}

BEGIN {
	env_heap_index = 0
}
