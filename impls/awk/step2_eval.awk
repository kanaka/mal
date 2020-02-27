@include "types.awk"
@include "reader.awk"
@include "printer.awk"

function READ(str)
{
	return reader_read_str(str)
}

function eval_ast(ast, env,    i, idx, len, new_idx, ret)
{
	switch (ast) {
	case /^'/:
		if (ast in env) {
			return types_addref(env[ast])
		}
		return "!\"'" substr(ast, 2) "' not found"
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

function EVAL(ast, env,    new_ast, ret, idx, f, f_idx)
{
	if (ast !~ /^\(/) {
		ret = eval_ast(ast, env)
		types_release(ast)
		return ret
	}
	idx = substr(ast, 2)
	if (types_heap[idx]["len"] == 0) {
		return ast
	}
	new_ast = eval_ast(ast, env)
	types_release(ast)
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
	repl_env["'+"] = "&add"
	repl_env["'-"] = "&subtract"
	repl_env["'*"] = "&multiply"
	repl_env["'/"] = "&divide"
	env_builtinnames["add"] = "+"
	env_builtinnames["subtract"] = "-"
	env_builtinnames["multiply"] = "*"
	env_builtinnames["divide"] = "/"

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
	exit(0)
}
