function printer_pr_list(expr, print_readably,    idx, len, i, str)
{
	idx = substr(expr, 2)
	len = types_heap[idx]["len"]
	for (i = 0; i < len; ++i) {
		str = str printer_pr_str(types_heap[idx][i], print_readably) " "
	}
	return substr(str, 1, length(str) - 1)
}

function printer_pr_hash(expr, print_readably,    idx, var, str)
{
	idx = substr(expr, 2)
	for (var in types_heap[idx]) {
		switch (var) {
		case /^"/:
			str = str printer_pr_string(var, print_readably) " " printer_pr_str(types_heap[idx][var], print_readably) " "
			break
		case /^:/:
			str = str substr(var, 2) " " printer_pr_str(types_heap[idx][var], print_readably) " "
			break
		}
	}
	return substr(str, 1, length(str) - 1)
}

function printer_pr_string(expr, print_readably,    v, r)
{
	if (!print_readably) {
		return substr(expr, 2)
	}
	expr = substr(expr, 2)
	while (match(expr, /["\n\\]/, r)) {
		v = v substr(expr, 1, RSTART - 1) (r[0] == "\n" ? "\\n" : "\\" r[0])
		expr = substr(expr, RSTART + RLENGTH)
	}
	return "\"" v expr "\""
}

function printer_pr_str(expr, print_readably,    var)
{
	switch (expr) {
	case /^\(/:
		return "(" printer_pr_list(expr, print_readably) ")"
	case /^\[/:
		return "[" printer_pr_list(expr, print_readably) "]"
	case /^\{/:
		return "{" printer_pr_hash(expr, print_readably) "}"
	case /^"/:
		return printer_pr_string(expr, print_readably)
	case /^\$/:
		var = substr(expr, 2)
		return "#<Function> (fn* " printer_pr_str(types_heap[var]["params"], print_readably) " " printer_pr_str(types_heap[var]["body"], print_readably) ")"
	case /^&/:
		return "#<BuiltinFunction " env_builtinnames[substr(expr, 2)] ">"
	case /^%/:
		return "#<BuiltinFunction " env_builtinnames[types_heap[substr(expr, 2)]["func"]] ">"
	case /^\?/:
		return "(atom " printer_pr_str(types_heap[substr(expr, 2)]["obj"], print_readably) ")"
	default:
		return substr(expr, 2)
	}
}
