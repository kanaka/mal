@load "readfile"
@load "time"

function core_eq_sub(lhs, rhs,    i, len)
{
	if (lhs ~ /^[([]/ && rhs ~ /^[([]/) {
		lhs = substr(lhs, 2)
		rhs = substr(rhs, 2)
		len = types_heap[lhs]["len"]
		if (len != types_heap[rhs]["len"]) {
			return 0
		}
		for (i = 0; i < len; ++i) {
			if (!core_eq_sub(types_heap[lhs][i], types_heap[rhs][i])) {
				return 0
			}
		}
		return 1
	} else if (lhs ~ /^\{/ && rhs ~ /^\{/) {
		lhs = substr(lhs, 2)
		rhs = substr(rhs, 2)
		if (length(types_heap[lhs]) != length(types_heap[rhs])) {
			return 0
		}
		for (i in types_heap[lhs]) {
			if (types_heap[lhs][i] ~ /^["':+#([{?&$%]/ &&
				!core_eq_sub(types_heap[lhs][i], types_heap[rhs][i])) {
				return 0
			}
		}
		return 1
	} else {
		return lhs == rhs
	}
}

function core_eq(idx)
{
	if (types_heap[idx]["len"] != 3) {
		return "!\"Invalid argument length for builtin function '='. Expects exactly 2 arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	return core_eq_sub(types_heap[idx][1], types_heap[idx][2]) ? "#true" : "#false"
}

function core_throw(idx)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'throw'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	return "!" types_addref(types_heap[idx][1])
}



function core_nilp(idx)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'nil?'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	return types_heap[idx][1] == "#nil" ? "#true" : "#false"
}

function core_truep(idx)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'true?'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	return types_heap[idx][1] == "#true" ? "#true" : "#false"
}

function core_falsep(idx)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'false?'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	return types_heap[idx][1] == "#false" ? "#true" : "#false"
}

function core_stringp(idx)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'string?'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	return types_heap[idx][1] ~ /^"/ ? "#true" : "#false"
}

function core_symbol(idx,    str)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'symbol'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	str = types_heap[idx][1]
	if (str !~ /^"/) {
		return "!\"Incompatible type for argument 1 of builtin function 'symbol'. Expects string, supplied " types_typename(str) "."
	}
	return "'" substr(str, 2)
}

function core_symbolp(idx)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'symbol?'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	return types_heap[idx][1] ~ /^'/ ? "#true" : "#false"
}

function core_keyword(idx,    str)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'keyword'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	str = types_heap[idx][1]
	switch (str) {
	case /^:/:
		return str
	case /^"/:
		return "::" substr(str, 2)
	}
	return "!\"Incompatible type for argument 1 of builtin function 'keyword'. Expects string or keyword, supplied " types_typename(str) "."
}

function core_keywordp(idx)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'keyword?'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	return types_heap[idx][1] ~ /^:/ ? "#true" : "#false"
}



function core_pr_str(idx,    i, len, result)
{
	len = types_heap[idx]["len"]
	for (i = 1; i < len; ++i) {
		result = result printer_pr_str(types_heap[idx][i], 1) " "
	}
	return "\"" substr(result, 1, length(result) - 1)
}

function core_str(idx,    i, len, result)
{
	len = types_heap[idx]["len"]
	for (i = 1; i < len; ++i) {
		result = result printer_pr_str(types_heap[idx][i], 0)
	}
	return "\"" result
}

function core_prn(idx,    i, len, result)
{
	len = types_heap[idx]["len"]
	for (i = 1; i < len; ++i) {
		result = result printer_pr_str(types_heap[idx][i], 1) " "
	}
	print substr(result, 1, length(result) - 1)
	return "#nil"
}

function core_println(idx,    i, len, result)
{
	len = types_heap[idx]["len"]
	for (i = 1; i < len; ++i) {
		result = result printer_pr_str(types_heap[idx][i], 0) " "
	}
	print substr(result, 1, length(result) - 1)
	return "#nil"
}

function core_read_string(idx,    str)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'read-string'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	str = types_heap[idx][1]
	if (str !~ /^"/) {
		return "!\"Incompatible type for argument 1 of builtin function 'read-string'. Expects string, supplied " types_typename(str) "."
	}
	return reader_read_str(substr(str, 2))
}

function core_readline(idx,    prompt, var)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'readline'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	prompt = types_heap[idx][1]
	if (prompt !~ /^"/) {
		return "!\"Incompatible type for argument 1 of builtin function 'readline'. Expects string, supplied " types_typename(prompt) "."
	}
	printf("%s", printer_pr_str(prompt, 0))
	return getline var <= 0 ? "#nil" : "\"" var
}

function core_slurp(idx,    filename, str)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'slurp'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	filename = types_heap[idx][1]
	if (filename !~ /^"/) {
		return "!\"Incompatible type for argument 1 of builtin function 'slurp'. Expects string, supplied " types_typename(filename) "."
	}
	str = readfile(substr(filename, 2))
	if (str == "" && ERRNO != "") {
		return "!\"cannot read file '" filename "', ERRNO = " ERRNO
	}
	return "\"" str
}



function core_lt(idx,    lhs, rhs)
{
	if (types_heap[idx]["len"] != 3) {
		return "!\"Invalid argument length for builtin function '<'. Expects exactly 2 arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	lhs = types_heap[idx][1]
	if (lhs !~ /^\+/) {
		return "!\"Incompatible type for argument 1 of builtin function '<'. Expects number, supplied " types_typename(lhs) "."
	}
	rhs = types_heap[idx][2]
	if (rhs !~ /^\+/) {
		return "!\"Incompatible type for argument 2 of builtin function '<'. Expects number, supplied " types_typename(rhs) "."
	}
	return substr(lhs, 2) + 0 < substr(rhs, 2) + 0 ? "#true" : "#false"
}

function core_le(idx,    lhs, rhs)
{
	if (types_heap[idx]["len"] != 3) {
		return "!\"Invalid argument length for builtin function '<='. Expects exactly 2 arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	lhs = types_heap[idx][1]
	if (lhs !~ /^\+/) {
		return "!\"Incompatible type for argument 1 of builtin function '<='. Expects number, supplied " types_typename(lhs) "."
	}
	rhs = types_heap[idx][2]
	if (rhs !~ /^\+/) {
		return "!\"Incompatible type for argument 2 of builtin function '<='. Expects number, supplied " types_typename(rhs) "."
	}
	return substr(lhs, 2) + 0 <= substr(rhs, 2) + 0 ? "#true" : "#false"
}

function core_gt(idx,    lhs, rhs)
{
	if (types_heap[idx]["len"] != 3) {
		return "!\"Invalid argument length for builtin function '>'. Expects exactly 2 arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	lhs = types_heap[idx][1]
	if (lhs !~ /^\+/) {
		return "!\"Incompatible type for argument 1 of builtin function '>'. Expects number, supplied " types_typename(lhs) "."
	}
	rhs = types_heap[idx][2]
	if (rhs !~ /^\+/) {
		return "!\"Incompatible type for argument 2 of builtin function '>'. Expects number, supplied " types_typename(rhs) "."
	}
	return substr(lhs, 2) + 0 > substr(rhs, 2) + 0 ? "#true" : "#false"
}

function core_ge(idx,    lhs, rhs)
{
	if (types_heap[idx]["len"] != 3) {
		return "!\"Invalid argument length for builtin function '>='. Expects exactly 2 arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	lhs = types_heap[idx][1]
	if (lhs !~ /^\+/) {
		return "!\"Incompatible type for argument 1 of builtin function '>='. Expects number, supplied " types_typename(lhs) "."
	}
	rhs = types_heap[idx][2]
	if (rhs !~ /^\+/) {
		return "!\"Incompatible type for argument 2 of builtin function '>='. Expects number, supplied " types_typename(rhs) "."
	}
	return substr(lhs, 2) + 0 >= substr(rhs, 2) + 0 ? "#true" : "#false"
}

function core_add(idx,    lhs, rhs)
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

function core_subtract(idx,    lhs, rhs)
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

function core_multiply(idx,    lhs, rhs)
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

function core_divide(idx,    lhs, rhs)
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

function core_time_ms(idx)
{
	if (types_heap[idx]["len"] != 1) {
		return "!\"Invalid argument length for builtin function 'time-ms'. Expects no arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	return "+" int(gettimeofday() * 1000)
}



function core_list(idx,    new_idx, len, i)
{
	new_idx = types_allocate()
	len = types_heap[idx]["len"]
	for (i = 1; i < len; ++i) {
		types_addref(types_heap[new_idx][i - 1] = types_heap[idx][i])
	}
	types_heap[new_idx]["len"] = len - 1
	return "(" new_idx
}

function core_listp(idx)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'list?'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	return types_heap[idx][1] ~ /^\(/ ? "#true" : "#false"
}

function core_vector(idx,    new_idx, len, i)
{
	new_idx = types_allocate()
	len = types_heap[idx]["len"]
	for (i = 1; i < len; ++i) {
		types_addref(types_heap[new_idx][i - 1] = types_heap[idx][i])
	}
	types_heap[new_idx]["len"] = len - 1
	return "[" new_idx
}

function core_vectorp(idx)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'vector?'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	return types_heap[idx][1] ~ /^\[/ ? "#true" : "#false"
}

function core_hash_map(idx,    len, new_idx, i, key)
{
	len = types_heap[idx]["len"]
	if (len % 2 != 1) {
		return "!\"Invalid argument length for builtin function 'hash-map'. Expects even number of arguments, supplied " (len - 1) "."
	}
	new_idx = types_allocate()
	for (i = 1; i < len; i += 2) {
		key = types_heap[idx][i]
		if (key !~ /^[":]/) {
			types_release("{" new_idx)
			return "!\"Incompatible type for key argument of builtin function 'hash-map'. Expects string or keyword, supplied " types_typename(key) "."
		}
		if (key in types_heap[new_idx]) {
			types_release(types_heap[new_idx][key])
		}
		types_addref(types_heap[new_idx][key] = types_heap[idx][i + 1])
	}
	return "{" new_idx
}

function core_mapp(idx)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'map?'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	return types_heap[idx][1] ~ /^\{/ ? "#true" : "#false"
}

function core_assoc(idx,    len, map, i, key, add_list, new_idx, map_idx)
{
	len = types_heap[idx]["len"]
	if (len % 2 != 0) {
		return "!\"Invalid argument length for builtin function 'assoc'. Expects odd number of arguments, supplied " (len - 1) "."
	}
	map = types_heap[idx][1]
	if (map !~ /^\{/) {
		return "!\"Incompatible type for argument 1 of builtin function 'assoc'. Expects hash-map, supplied " types_typename(map) "."
	}
	for (i = 2; i < len; i += 2) {
		key = types_heap[idx][i]
		if (key !~ /^[":]/) {
			return "!\"Incompatible type for key argument of builtin function 'assoc'. Expects string or keyword, supplied " types_typename(key) "."
		}
		add_list[key] = types_heap[idx][i + 1]
	}
	new_idx = types_allocate()
	map_idx = substr(map, 2)
	for (key in types_heap[map_idx]) {
		if (key ~ /^[":]|^meta$/ && !(key in add_list)) {
			types_addref(types_heap[new_idx][key] = types_heap[map_idx][key])
		}
	}
	for (key in add_list) {
		types_addref(types_heap[new_idx][key] = add_list[key])
	}
	return "{" new_idx
}

function core_dissoc(idx,    len, map, i, key, del_list, new_idx, map_idx)
{
	len = types_heap[idx]["len"]
	if (len < 2) {
		return "!\"Invalid argument length for builtin function 'dissoc'. Expects at least 1 argument, supplied " (len - 1) "."
	}
	map = types_heap[idx][1]
	if (map !~ /^\{/) {
		return "!\"Incompatible type for argument 1 of builtin function 'dissoc'. Expects hash-map, supplied " types_typename(map) "."
	}
	for (i = 2; i < len; ++i) {
		key = types_heap[idx][i]
		if (key !~ /^[":]/) {
			return "!\"Incompatible type for key argument of builtin function 'dissoc'. Expects string or keyword, supplied " types_typename(key) "."
		}
		del_list[key] = "1"
	}
	new_idx = types_allocate()
	map_idx = substr(map, 2)
	for (key in types_heap[map_idx]) {
		if (key ~ /^[":]|^meta$/ && !(key in del_list)) {
			types_addref(types_heap[new_idx][key] = types_heap[map_idx][key])
		}
	}
	return "{" new_idx
}

function core_get(idx,    map, key, map_idx)
{
	if (types_heap[idx]["len"] != 3) {
		return "!\"Invalid argument length for builtin function 'get'. Expects exactly 2 arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	map = types_heap[idx][1]
	if (map !~ /^\{/ && map != "#nil") {
		return "!\"Incompatible type for argument 1 of builtin function 'get'. Expects hash-map or nil, supplied " types_typename(map) "."
	}
	key = types_heap[idx][2]
	if (key !~ /^[":]/) {
		return "!\"Incompatible type for argument 2 of builtin function 'get'. Expects string or keyword, supplied " types_typename(key) "."
	}
	if (map != "#nil" && key in types_heap[map_idx = substr(map, 2)]) {
		return types_addref(types_heap[map_idx][key])
	} else {
		return "#nil"
	}
}

function core_containsp(idx,    map, key)
{
	if (types_heap[idx]["len"] != 3) {
		return "!\"Invalid argument length for builtin function 'contains?'. Expects exactly 2 arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	map = types_heap[idx][1]
	if (map !~ /^\{/) {
		return "!\"Incompatible type for argument 1 of builtin function 'contains?'. Expects hash-map, supplied " types_typename(map) "."
	}
	key = types_heap[idx][2]
	if (key !~ /^[":]/) {
		return "!\"Incompatible type for argument 2 of builtin function 'contains?'. Expects string or keyword, supplied " types_typename(key) "."
	}
	return key in types_heap[substr(map, 2)] ? "#true" : "#false"
}

function core_keys(idx,    map, map_idx, new_idx, len, key)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'keys'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	map = types_heap[idx][1]
	if (map !~ /^\{/) {
		return "!\"Incompatible type for argument 1 of builtin function 'keys'. Expects hash-map, supplied " types_typename(map) "."
	}
	map_idx = substr(map, 2)
	new_idx = types_allocate()
	len = 0
	for (key in types_heap[map_idx]) {
		if (key ~ /^[":]/) {
			types_heap[new_idx][len++] = key
		}
	}
	types_heap[new_idx]["len"] = len
	return "(" new_idx
}

function core_vals(idx,    map, map_idx, new_idx, len, key)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'vals'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	map = types_heap[idx][1]
	if (map !~ /^\{/) {
		return "!\"Incompatible type for argument 1 of builtin function 'vals'. Expects hash-map, supplied " types_typename(map) "."
	}
	map_idx = substr(map, 2)
	new_idx = types_allocate()
	len = 0
	for (key in types_heap[map_idx]) {
		if (key ~ /^[":]/) {
			types_addref(types_heap[new_idx][len++] = types_heap[map_idx][key])
		}
	}
	types_heap[new_idx]["len"] = len
	return "(" new_idx
}



function core_sequentialp(idx)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'sequential?'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	return types_heap[idx][1] ~ /^[([]/ ? "#true" : "#false"
}

function core_cons(idx,    lst, lst_idx, new_idx, len, i)
{
	if (types_heap[idx]["len"] != 3) {
		return "!\"Invalid argument length for builtin function 'cons'. Expects exactly 2 arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	lst = types_heap[idx][2]
	if (lst !~ /^[([]/) {
		return "!\"Incompatible type for argument 1 of builtin function 'cons'. Expects list or vector, supplied " types_typename(lst) "."
	}
	lst_idx = substr(lst, 2)
	new_idx = types_allocate()
	types_addref(types_heap[new_idx][0] = types_heap[idx][1])
	len = types_heap[lst_idx]["len"]
	for (i = 0; i < len; ++i) {
		types_addref(types_heap[new_idx][i + 1] = types_heap[lst_idx][i])
	}
	types_heap[new_idx]["len"] = len + 1
	return "(" new_idx
}

function core_concat(idx,    new_idx, new_len, len, i, lst, lst_idx, lst_len, j)
{
	new_idx = types_allocate()
	new_len = 0
	len = types_heap[idx]["len"]
	for (i = 1; i < len; ++i) {
		lst = types_heap[idx][i]
		if (lst !~ /^[([]/) {
			types_heap[new_idx]["len"] = new_len
			types_release("(" new_idx)
			return "!\"Incompatible type for argument ' (i - 1) ' of builtin function 'concat'. Expects list or vector, supplied " types_typename(lst) "."
		}
		lst_idx = substr(lst, 2)
		lst_len = types_heap[lst_idx]["len"]
		for (j = 0; j < lst_len; ++j) {
			types_addref(types_heap[new_idx][new_len++] = types_heap[lst_idx][j])
		}
	}
	types_heap[new_idx]["len"] = new_len
	return "(" new_idx
}

function core_nth(idx,    lst, num, n, lst_idx)
{
	if (types_heap[idx]["len"] != 3) {
		return "!\"Invalid argument length for builtin function 'nth'. Expects exactly 2 arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	lst = types_heap[idx][1]
	if (lst !~ /^[([]/) {
		return "!\"Incompatible type for argument 1 of builtin function 'nth'. Expects list or vector, supplied " types_typename(lst) "."
	}
	num = types_heap[idx][2]
	if (num !~ /^\+/) {
		return "!\"Incompatible type for argument 2 of builtin function 'nth'. Expects number, supplied " types_typename(num) "."
	}
	n = substr(num, 2) + 0
	lst_idx = substr(lst, 2)
	if (n < 0 || types_heap[lst_idx]["len"] <= n) {
		return "!\"Index out of range. Sequence length is " types_heap[lst_idx]["len"] ", supplied " n "."
	}
	return types_addref(types_heap[lst_idx][n])
}

function core_first(idx,    lst, lst_idx)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'first'. Expects exactly 2 arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	lst = types_heap[idx][1]
	if (lst == "#nil") {
	        return "#nil"
	}
	if (lst !~ /^[([]/) {
		return "!\"Incompatible type for argument 1 of builtin function 'first'. Expects list, vector or nil, supplied " types_typename(lst) "."
	}
	lst_idx = substr(lst, 2)
	return types_heap[lst_idx]["len"] == 0 ? "#nil" : types_addref(types_heap[lst_idx][0])
}

function core_rest(idx,    lst, lst_idx, lst_len, new_idx, i)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'rest'. Expects exactly 2 arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	lst = types_heap[idx][1]
	if (lst == "#nil") {
	        new_idx = types_allocate()
	        types_heap[new_idx]["len"] = 0
	        return "(" new_idx
	}
	if (lst !~ /^[([]/) {
		return "!\"Incompatible type for argument 1 of builtin function 'rest'. Expects list, vector or nil, supplied " types_typename(lst) "."
	}
	lst_idx = substr(lst, 2)
	lst_len = types_heap[lst_idx]["len"]
	new_idx = types_allocate()
	for (i = 1; i < lst_len; ++i) {
		types_addref(types_heap[new_idx][i - 1] = types_heap[lst_idx][i])
	}
	types_heap[new_idx]["len"] = lst_len - 1
	return "(" new_idx
}

function core_emptyp(idx,    lst)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'empty?'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	lst = types_heap[idx][1]
	if (lst !~ /^[([]/) {
		return "!\"Incompatible type for argument 1 of builtin function 'empty?'. Expects list or vector, supplied " types_typename(lst) "."
	}
	return types_heap[substr(lst, 2)]["len"] == 0 ? "#true" : "#false"
}

function core_count(idx,    lst)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'count'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	lst = types_heap[idx][1]
	if (lst ~ /^[([]/) {
		return "+" types_heap[substr(lst, 2)]["len"]
	}
	if (lst == "#nil") {
		return "+0"
	}
	return "!\"Incompatible type for argument 1 of builtin function 'count'. Expects list, vector or nil, supplied " types_typename(lst) "."
}

function core_apply(idx,    len, f, lst, new_idx, i, lst_idx, lst_len, f_idx, env, ret)
{
	len = types_heap[idx]["len"]
	if (len < 3) {
		return "!\"Invalid argument length for builtin function 'apply'. Expects at least 2 arguments, supplied " (len - 1) "."
	}
	f = types_heap[idx][1]
	if (f !~ /^[$&%]/) {
		return "!\"Incompatible type for argument 1 of builtin function 'apply'. Expects function, supplied " types_typename(f) "."
	}
	lst = types_heap[idx][len - 1]
	if (lst !~ /^[([]/) {
		return "!\"Incompatible type for argument ' (len - 1) ' of builtin function 'apply'. Expects list or vector, supplied " types_typename(lst) "."
	}

	new_idx = types_allocate()
	types_addref(types_heap[new_idx][0] = f)
	for (i = 2; i < len - 1; ++i) {
		types_addref(types_heap[new_idx][i - 1] = types_heap[idx][i])
	}
	lst_idx = substr(lst, 2)
	lst_len = types_heap[lst_idx]["len"]
	for (i = 0; i < lst_len; ++i) {
		types_addref(types_heap[new_idx][len + i - 2] = types_heap[lst_idx][i])
	}
	types_heap[new_idx]["len"] = len + lst_len - 2

	f_idx = substr(f, 2)
	switch (f) {
	case /^\$/:
		env = env_new(types_heap[f_idx]["env"], types_heap[f_idx]["params"], new_idx)
		types_release("(" new_idx)
		if (env ~ /^!/) {
			return env
		}
		ret = EVAL(types_addref(types_heap[f_idx]["body"]), env)
		env_release(env)
		return ret
	case /^%/:
		f_idx = types_heap[f_idx]["func"]
	case /^&/:
		ret = @f_idx(new_idx)
		types_release("(" new_idx)
		return ret
	}
}

function core_map(idx,    f, lst, f_idx, lst_idx, lst_len, new_idx, expr_idx, i, env, ret, val)
{
	if (types_heap[idx]["len"] != 3) {
		return "!\"Invalid argument length for builtin function 'map'. Expects exactly 2 arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	f = types_heap[idx][1]
	if (f !~ /^[$&%]/) {
		return "!\"Incompatible type for argument 1 of builtin function 'map'. Expects function, supplied " types_typename(f) "."
	}
	lst = types_heap[idx][2]
	if (lst !~ /^[([]/) {
		return "!\"Incompatible type for argument 2 of builtin function 'map'. Expects list or vector, supplied " types_typename(lst) "."
	}
	f_idx = substr(f, 2)
	lst_idx = substr(lst, 2)
	lst_len = types_heap[lst_idx]["len"]
	new_idx = types_allocate()
	types_heap[new_idx][0] = f
	types_heap[new_idx]["len"] = 2
	expr_idx = types_allocate()
	for (i = 0; i < lst_len; ++i) {
		types_heap[new_idx][1] = types_heap[lst_idx][i]
		switch (f) {
		case /^\$/:
			env = env_new(types_heap[f_idx]["env"], types_heap[f_idx]["params"], new_idx)
			if (env ~ /^!/) {
				types_heap[expr_idx]["len"] = i
				types_heap[new_idx]["len"] = 0
				types_release("(" expr_idx)
				types_release("(" new_idx)
				return env
			}
			ret = EVAL(types_addref(types_heap[f_idx]["body"]), env)
			env_release(env)
			break
		case /^%/:
			f_idx = types_heap[f_idx]["func"]
		case /^&/:
			ret = @f_idx(new_idx)
			break
		}
		if (ret ~ /^!/) {
			types_heap[expr_idx]["len"] = i
			types_heap[new_idx]["len"] = 0
			types_release("(" expr_idx)
			types_release("(" new_idx)
			return ret
		}
		types_heap[expr_idx][i] = ret
	}
	types_heap[expr_idx]["len"] = lst_len
	types_heap[new_idx]["len"] = 0
	types_release("(" new_idx)
	return "(" expr_idx
}



function core_conj(idx,    len, lst, lst_idx, lst_len, new_idx, i, j)
{
	len = types_heap[idx]["len"]
	if (len < 3) {
		return "!\"Invalid argument length for builtin function 'conj'. Expects at least 2 arguments, supplied " (len - 1) "."
	}
	lst = types_heap[idx][1]
	if (lst !~ /^[([]/) {
		return "!\"Incompatible type for argument 1 of builtin function 'conj'. Expects list or vector, supplied " types_typename(lst) "."
	}
	lst_idx = substr(lst, 2)
	lst_len = types_heap[lst_idx]["len"]
	new_idx = types_allocate()
	j = 0
	if (lst ~ /^\(/) {
		for (i = len - 1; i >= 2; --i) {
			types_addref(types_heap[new_idx][j++] = types_heap[idx][i])
		}
		for (i = 0; i < lst_len; ++i) {
			types_addref(types_heap[new_idx][j++] = types_heap[lst_idx][i])
		}
	} else {
		for (i = 0; i < lst_len; ++i) {
			types_addref(types_heap[new_idx][j++] = types_heap[lst_idx][i])
		}
		for (i = 2; i < len; ++i) {
			types_addref(types_heap[new_idx][j++] = types_heap[idx][i])
		}
	}
	types_addref(types_heap[new_idx]["meta"] = types_heap[lst_idx]["meta"])
	types_heap[new_idx]["len"] = j
	return substr(lst, 1, 1) new_idx
}

function core_seq(idx,     obj, obj_idx, new_idx, i, len, chars)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'seq'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	obj = types_heap[idx][1]
	if (obj ~ /^[(]/) {
		if (types_heap[substr(obj, 2)]["len"] == 0) {
			return "#nil"
		}
		return types_addref(obj)
	} else if (obj ~ /^\[/) {
		obj_idx = substr(obj, 2)
		len = types_heap[obj_idx]["len"]
		if (len == 0) { return "#nil" }
		new_idx = types_allocate()
		for (i = 0; i < len; ++i) {
			types_addref(types_heap[new_idx][i] = types_heap[obj_idx][i])
		}
		types_heap[new_idx]["len"] = len
		return "(" new_idx
	} else if (obj ~ /^"/) {
		obj_idx = substr(obj, 2)
		len = length(obj_idx)
		if (len == 0) { return "#nil" }
		new_idx = types_allocate()
		split(obj_idx, chars, "")
		for (i = 0; i <= len; ++i) {
			types_heap[new_idx][i] = "\"" chars[i+1]
		}
		types_heap[new_idx]["len"] = len
		return "(" new_idx
	} else if (obj == "#nil") {
		return "#nil"
	} else {
		return "!\"seq: called on non-sequence"
	}
}


function core_meta(idx,    obj, obj_idx)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'meta'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	obj = types_heap[idx][1]
	if (obj ~ /^[([{$%]/ && "meta" in types_heap[obj_idx = substr(obj, 2)]) {
		return types_addref(types_heap[obj_idx]["meta"])
	}
	return "#nil"
}

function core_with_meta(idx,    obj, obj_idx, new_idx, i, len)
{
	if (types_heap[idx]["len"] != 3) {
		return "!\"Invalid argument length for builtin function 'with-meta'. Expects exactly 2 arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	obj = types_heap[idx][1]
	obj_idx = substr(obj, 2)
	new_idx = types_allocate()
	types_addref(types_heap[new_idx]["meta"] = types_heap[idx][2])
	switch (obj) {
	case /^[([]/:
		len = types_heap[obj_idx]["len"]
		for (i = 0; i < len; ++i) {
			types_addref(types_heap[new_idx][i] = types_heap[obj_idx][i])
		}
		types_heap[new_idx]["len"] = len
		return substr(obj, 1, 1) new_idx
	case /^\{/:
		for (i in types_heap[obj_idx]) {
			if (i ~ /^[":]/) {
				types_addref(types_heap[new_idx][i] = types_heap[obj_idx][i])
			}
		}
		return "{" new_idx
	case /^\$/:
		types_addref(types_heap[new_idx]["params"] = types_heap[obj_idx]["params"])
		types_addref(types_heap[new_idx]["body"] = types_heap[obj_idx]["body"])
		env_addref(types_heap[new_idx]["env"] = types_heap[obj_idx]["env"])
		return "$" new_idx
	case /^&/:
		types_heap[new_idx]["func"] = obj_idx
		return "%" new_idx
	case /^%/:
		types_heap[new_idx]["func"] = types_heap[obj_idx]["func"]
		return "%" new_idx
	default:
		types_release("{" new_idx)
		return "!\"Incompatible type for argument 1 of builtin function 'with-meta'. Expects list, vector, hash-map or function, supplied " types_typename(lst) "."
	}
}

function core_atom(idx,    atom_idx)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'atom'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	atom_idx = types_allocate()
	types_addref(types_heap[atom_idx]["obj"] = types_heap[idx][1])
	return "?" atom_idx
}

function core_atomp(idx)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'atom?'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	return types_heap[idx][1] ~ /^\?/ ? "#true" : "#false"
}

function core_deref(idx,    atom)
{
	if (types_heap[idx]["len"] != 2) {
		return "!\"Invalid argument length for builtin function 'deref'. Expects exactly 1 argument, supplied " (types_heap[idx]["len"] - 1) "."
	}
	atom = types_heap[idx][1]
	if (atom !~ /^\?/) {
		return "!\"Incompatible type for argument 1 of builtin function 'deref'. Expects atom, supplied " types_typename(atom) "."
	}
	return types_addref(types_heap[substr(atom, 2)]["obj"])
}

function core_reset(idx,    atom, atom_idx)
{
	if (types_heap[idx]["len"] != 3) {
		return "!\"Invalid argument length for builtin function 'reset!'. Expects exactly 2 arguments, supplied " (types_heap[idx]["len"] - 1) "."
	}
	atom = types_heap[idx][1]
	if (atom !~ /^\?/) {
		return "!\"Incompatible type for argument 1 of builtin function 'reset!'. Expects atom, supplied " types_typename(atom) "."
	}
	atom_idx = substr(atom, 2)
	types_release(types_heap[atom_idx]["obj"])
	return types_addref(types_heap[atom_idx]["obj"] = types_heap[idx][2])
}

function core_swap(idx,    expr,    atom, f, lst_idx, ret, f_idx, env, i, len, atom_idx)
{
	len = types_heap[idx]["len"]
	if (len < 3) {
		return "!\"Invalid argument length for builtin function 'swap!'. Expects at least 2 arguments, supplied " (len - 1) "."
	}
	atom = types_heap[idx][1]
	if (atom !~ /^\?/) {
		return "!\"Incompatible type for argument 1 of builtin function 'swap!'. Expects atom, supplied " types_typename(atom) "."
	}
	f = types_heap[idx][2]
	if (f !~ /^[&$%]/) {
		return "!\"Incompatible type for argument 2 of builtin function 'swap!'. Expects function, supplied " types_typename(f) "."
	}
	lst_idx = types_allocate()
	atom_idx = substr(atom, 2)
	types_addref(types_heap[lst_idx][0] = f)
	types_addref(types_heap[lst_idx][1] = types_heap[atom_idx]["obj"])
	for (i = 3; i < len; ++i) {
		types_addref(types_heap[lst_idx][i - 1] = types_heap[idx][i])
	}
	types_heap[lst_idx]["len"] = len - 1

	f_idx = substr(f, 2)
	switch (f) {
	case /^\$/:
		env = env_new(types_heap[f_idx]["env"], types_heap[f_idx]["params"], lst_idx)
		types_release("(" lst_idx)
		if (env ~ /^!/) {
			return env
		}
		ret = EVAL(types_addref(types_heap[f_idx]["body"]), env)
		env_release(env)
		break
	case /^%/:
		f_idx = types_heap[f_idx]["func"]
	case /^&/:
		ret = @f_idx(lst_idx)
		types_release("(" lst_idx)
		break
	}

	if (ret ~ /^!/) {
		return ret
	}
	types_release(types_heap[atom_idx]["obj"])
	return types_addref(types_heap[atom_idx]["obj"] = ret)
}

function core_init()
{
	core_ns["'="] = "&core_eq"
	core_ns["'throw"] = "&core_throw"

	core_ns["'nil?"] = "&core_nilp"
	core_ns["'true?"] = "&core_truep"
	core_ns["'false?"] = "&core_falsep"
	core_ns["'string?"] = "&core_stringp"
	core_ns["'symbol"] = "&core_symbol"
	core_ns["'symbol?"] = "&core_symbolp"
	core_ns["'keyword"] = "&core_keyword"
	core_ns["'keyword?"] = "&core_keywordp"

	core_ns["'pr-str"] = "&core_pr_str"
	core_ns["'str"] = "&core_str"
	core_ns["'prn"] = "&core_prn"
	core_ns["'println"] = "&core_println"
	core_ns["'read-string"] = "&core_read_string"
	core_ns["'readline"] = "&core_readline"
	core_ns["'slurp"] = "&core_slurp"

	core_ns["'<"] = "&core_lt"
	core_ns["'<="] = "&core_le"
	core_ns["'>"] = "&core_gt"
	core_ns["'>="] = "&core_ge"
	core_ns["'+"] = "&core_add"
	core_ns["'-"] = "&core_subtract"
	core_ns["'*"] = "&core_multiply"
	core_ns["'/"] = "&core_divide"
	core_ns["'time-ms"] = "&core_time_ms"

	core_ns["'list"] = "&core_list"
	core_ns["'list?"] = "&core_listp"
	core_ns["'vector"] = "&core_vector"
	core_ns["'vector?"] = "&core_vectorp"
	core_ns["'hash-map"] = "&core_hash_map"
	core_ns["'map?"] = "&core_mapp"
	core_ns["'assoc"] = "&core_assoc"
	core_ns["'dissoc"] = "&core_dissoc"
	core_ns["'get"] = "&core_get"
	core_ns["'contains?"] = "&core_containsp"
	core_ns["'keys"] = "&core_keys"
	core_ns["'vals"] = "&core_vals"

	core_ns["'sequential?"] = "&core_sequentialp"
	core_ns["'cons"] = "&core_cons"
	core_ns["'concat"] = "&core_concat"
	core_ns["'nth"] = "&core_nth"
	core_ns["'first"] = "&core_first"
	core_ns["'rest"] = "&core_rest"
	core_ns["'empty?"] = "&core_emptyp"
	core_ns["'count"] = "&core_count"
	core_ns["'apply"] = "&core_apply"
	core_ns["'map"] = "&core_map"

	core_ns["'conj"] = "&core_conj"
	core_ns["'seq"] = "&core_seq"

	core_ns["'meta"] = "&core_meta"
	core_ns["'with-meta"] = "&core_with_meta"
	core_ns["'atom"] = "&core_atom"
	core_ns["'atom?"] = "&core_atomp"
	core_ns["'deref"] = "&core_deref"
	core_ns["'reset!"] = "&core_reset"
	core_ns["'swap!"] = "&core_swap"
}



BEGIN {
	core_init()
}
