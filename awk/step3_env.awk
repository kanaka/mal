@include "types.awk"
@include "reader.awk"
@include "printer.awk"
@include "env.awk"

function READ(str)
{
	return reader_read_str(str)
}

function eval_ast(ast, env,    i, idx, len, new_idx, ret)
{
	switch (ast) {
	case /^'/:
		ret = env_get(env, ast)
		if (ret !~ /^!/) {
			types_addref(ret)
		}
		return ret
	case /^[([]/:
		idx = substr(ast, 2)
		len = types_heap[idx]["len"]
		new_idx = types_allocate()
		for (i = 0; i < len; ++i) {
			ret = EVAL(types_addref(types_heap[idx][i]), env)
			if (ret ~ /^!/) {
				types_heap[new_idx]["len"] = i
				types_release(substr(ast, 1, 1) new_idx)
				return ret
			}
			types_heap[new_idx][i] = ret
		}
		types_heap[new_idx]["len"] = len
		return substr(ast, 1, 1) new_idx
	case /^\{/:
		idx = substr(ast, 2)
		new_idx = types_allocate()
		for (i in types_heap[idx]) {
			if (i ~ /^[":]/) {
				ret = EVAL(types_addref(types_heap[idx][i]), env)
				if (ret ~ /^!/) {
					types_release("{" new_idx)
					return ret
				}
				types_heap[new_idx][i] = ret
			}
		}
		return "{" new_idx
	default:
		return ast
	}
}

function EVAL_def(ast, env,    idx, sym, ret, len)
{
	idx = substr(ast, 2)
	if (types_heap[idx]["len"] != 3) {
		len = types_heap[idx]["len"]
		types_release(ast)
		env_release(env)
		return "!\"Invalid argument length for 'def!'. Expects exactly 2 arguments, supplied" (len - 1) "."
	}
	sym = types_heap[idx][1]
	if (sym !~ /^'/) {
		types_release(ast)
		env_release(env)
		return "!\"Incompatible type for argument 1 of 'def!'. Expects symbol, supplied " types_typename(sym) "."
	}
	ret = EVAL(types_addref(types_heap[idx][2]), env)
	if (ret !~ /^!/) {
		env_set(env, sym, ret)
		types_addref(ret)
	}
	types_release(ast)
	env_release(env)
	return ret
}

function EVAL_let(ast, env,    idx, params, params_idx, params_len, new_env, i, sym, ret, body, len)
{
	idx = substr(ast, 2)
	if (types_heap[idx]["len"] != 3) {
		len = types_heap[idx]["len"]
		types_release(ast)
		env_release(env)
		return "!\"Invalid argument length for 'let*'. Expects exactly 2 arguments, supplied " (len - 1) "."
	}
	params = types_heap[idx][1]
	if (params !~ /^[([]/) {
		types_release(ast)
		env_release(env)
		return "!\"Incompatible type for argument 1 of 'let*'. Expects list or vector, supplied " types_typename(params) "."
	}
	params_idx = substr(params, 2)
	params_len = types_heap[params_idx]["len"]
	if (params_len % 2 != 0) {
		types_release(ast)
		env_release(env)
		return "!\"Invalid elements count for argument 1 of 'let*'. Expects even number of elements, supplied " params_len "."
	}
	new_env = env_new(env)
	env_release(env)
	for (i = 0; i < params_len; i += 2) {
		sym = types_heap[params_idx][i]
		if (sym !~ /^'/) {
			types_release(ast)
			env_release(new_env)
			return "!\"Incompatible type for odd element of argument 1 of 'let*'. Expects symbol, supplied " types_typename(sym) "."
		}
		ret = EVAL(types_addref(types_heap[params_idx][i + 1]), new_env)
		if (ret ~ /^!/) {
			types_release(ast)
			env_release(new_env)
			return ret
		}
		env_set(new_env, sym, ret)
	}
	types_addref(body = types_heap[idx][2])
	types_release(ast)
	ret = EVAL(body, new_env)
	env_release(new_env)
	return ret
}

function EVAL(ast, env,    new_ast, ret, idx, f, f_idx)
{
	env_addref(env)
	if (ast !~ /^\(/) {
		ret = eval_ast(ast, env)
		types_release(ast)
		env_release(env)
		return ret
	}
	idx = substr(ast, 2)
	if (types_heap[idx]["len"] == 0) {
		env_release(env)
		return ast
	}
	switch (types_heap[idx][0]) {
	case "'def!":
		return EVAL_def(ast, env)
	case "'let*":
		return EVAL_let(ast, env)
	default:
		new_ast = eval_ast(ast, env)
		types_release(ast)
		env_release(env)
		if (new_ast ~ /^!/) {
			return new_ast
		}
		idx = substr(new_ast, 2)
		f = types_heap[idx][0]
		if (f ~ /^&/) {
			f_idx = substr(f, 2)
			ret = @f_idx(idx)
			types_release(new_ast)
			return ret
		} else {
			types_release(new_ast)
			return "!\"First element of list must be function, supplied " types_typename(f) "."
		}
	}
}

function PRINT(expr,    str)
{
	str = printer_pr_str(expr, 1)
	types_release(expr)
	return str
}

function rep(str,    ast, expr)
{
	ast = READ(str)
	if (ast ~ /^!/) {
		return ast
	}
	expr = EVAL(ast, repl_env)
	if (expr ~ /^!/) {
		return expr
	}
	return PRINT(expr)
}

function add(idx,    lhs, rhs)
{
	if (types_heap[idx]["len"] != 3) {
		return "!\"Invalid argument length for builtin function '+'. Expects exactly 2 arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	lhs = types_heap[idx][1]
	if (lhs !~ /^\+/) {
		return "!\"Incompatible type for argument 1 of builtin function '+'. Expects number, supplied " types_typename(lhs) "."
	}
	rhs = types_heap[idx][2]
	if (rhs !~ /^\+/) {
		return "!\"Incompatible type for argument 2 of builtin function '+'. Expects number, supplied " types_typename(rhs) "."
	}
	return "+" (substr(lhs, 2) + substr(rhs, 2))
}

function subtract(idx,    lhs, rhs)
{
	if (types_heap[idx]["len"] != 3) {
		return "!\"Invalid argument length for builtin function '-'. Expects exactly 2 arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	lhs = types_heap[idx][1]
	if (lhs !~ /^\+/) {
		return "!\"Incompatible type for argument 1 of builtin function '-'. Expects number, supplied " types_typename(lhs) "."
	}
	rhs = types_heap[idx][2]
	if (rhs !~ /^\+/) {
		return "!\"Incompatible type for argument 2 of builtin function '-'. Expects number, supplied " types_typename(rhs) "."
	}
	return "+" (substr(lhs, 2) - substr(rhs, 2))
}

function multiply(idx,    lhs, rhs)
{
	if (types_heap[idx]["len"] != 3) {
		return "!\"Invalid argument length for builtin function '*'. Expects exactly 2 arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	lhs = types_heap[idx][1]
	if (lhs !~ /^\+/) {
		return "!\"Incompatible type for argument 1 of builtin function '*'. Expects number, supplied " types_typename(lhs) "."
	}
	rhs = types_heap[idx][2]
	if (rhs !~ /^\+/) {
		return "!\"Incompatible type for argument 2 of builtin function '*'. Expects number, supplied " types_typename(rhs) "."
	}
	return "+" (substr(lhs, 2) * substr(rhs, 2))
}

function divide(idx,    lhs, rhs)
{
	if (types_heap[idx]["len"] != 3) {
		return "!\"Invalid argument length for builtin function '/'. Expects exactly 2 arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	lhs = types_heap[idx][1]
	if (lhs !~ /^\+/) {
		return "!\"Incompatible type for argument 1 of builtin function '/'. Expects number, supplied " types_typename(lhs) "."
	}
	rhs = types_heap[idx][2]
	if (rhs !~ /^\+/) {
		return "!\"Incompatible type for argument 2 of builtin function '/'. Expects number, supplied " types_typename(rhs) "."
	}
	return "+" int(substr(lhs, 2) / substr(rhs, 2))
}

function main(str, ret)
{
	repl_env = env_new()
	env_set(repl_env, "'+", "&add")
	env_set(repl_env, "'-", "&subtract")
	env_set(repl_env, "'*", "&multiply")
	env_set(repl_env, "'/", "&divide")

	while (1) {
		printf("user> ")
		if (getline str <= 0) {
			break
		}
		ret = rep(str)
		if (ret ~ /^!/) {
			print "ERROR: " printer_pr_str(substr(ret, 2))
		} else {
			print ret
		}
	}
}

BEGIN {
	main()
	env_check(0)
	env_dump()
	types_dump()
	exit(0)
}
