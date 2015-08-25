
# string"
# symbol  '
# keyword :
# number  +
# nil     #
# true    #
# false   #
# list    (
# vector  [
# hash    {
# atom    ?
# builtin function           &
# builtin function with meta %
# user defined function      $

function types_allocate()
{
	types_heap[types_heap_index]["ref"] = 1
	return types_heap_index++
}

function types_addref(ast)
{
	if (ast ~ /^[([{$%?]/) {
		++types_heap[substr(ast, 2)]["ref"]
	}
	return ast
}

function types_release(ast,    idx, ref, i, len)
{
	switch (ast) {
	case /^[([]/:
		idx = substr(ast, 2)
		ref = --types_heap[idx]["ref"]
		if (ref <= 0) {
			if (ref < 0) {
				print "ref count error:" ast ", " ref
			}
			len = types_heap[idx]["len"]
			for (i = 0; i < len; ++i) {
				types_release(types_heap[idx][i])
			}
			types_release(types_heap[idx]["meta"])
			delete types_heap[idx]
		}
		return
	case /^\{/:
		idx = substr(ast, 2)
		ref = --types_heap[idx]["ref"]
		if (ref <= 0) {
			if (ref < 0) {
				print "ref count error:" ast ", " ref
			}
			for (i in types_heap[idx]) {
				if (i ~ /^[":]/) {
					types_release(types_heap[idx][i])
				}
			}
			types_release(types_heap[idx]["meta"])
			delete types_heap[idx]
		}
		return
	case /^\$/:
		idx = substr(ast, 2)
		ref = --types_heap[idx]["ref"]
		if (ref <= 0) {
			if (ref < 0) {
				print "ref count error:" ast ", " ref
			}
			types_release(types_heap[idx]["params"])
			types_release(types_heap[idx]["body"])
			types_release(types_heap[idx]["meta"])
			env_release(types_heap[idx]["env"])
			delete types_heap[idx]
		}
		return
	case /^%/:
		idx = substr(ast, 2)
		ref = --types_heap[idx]["ref"]
		if (ref <= 0) {
			if (ref < 0) {
				print "ref count error:" ast ", " ref
			}
			types_release(types_heap[idx]["meta"])
			delete types_heap[idx]
		}
		return
	case /^\?/:
		idx = substr(ast, 2)
		ref = --types_heap[idx]["ref"]
		if (ref <= 0) {
			if (ref < 0) {
				print "ref count error:" ast ", " ref
			}
			types_release(types_heap[idx]["obj"])
			delete types_heap[idx]
		}
	}
}

function types_check(val,    idx, len, i)
{
	if (val !~ /^[([{?%$]/) {
		return
	}
	idx = substr(val, 2)
	if (!(idx in types_heap)) {
		print "dangling reference " val
		return
	}
	if (types_heap[idx]["checked"]++) {
		return
	}
	#types_heap[idx]["checked"] = 1
	switch (val) {
	case /^[([]/:
		if (!("len" in types_heap[idx])) {
			print "length not found in " val
			return
		}
		len = types_heap[idx]["len"]
		for (i = 0; i < len; ++i) {
			if (!(i in types_heap[idx])) {
				print "sequence corrupted in " val " of " i
			} else {
				types_check(types_heap[idx][i])
			}
		}
		types_check(types_heap[idx]["meta"])
		return
	case /^\{/:
		for (i in types_heap[idx]) {
			if (i != "ref") {
				types_check(types_heap[idx][i])
			}
		}
		return
	case /^\?/:
		if (!("obj" in types_heap[idx])) {
			print "atom corrupted in " val
		} else {
			types_check(types_heap[idx]["obj"])
		}
		types_check(types_heap[idx]["meta"])
		return
	case /^%/:
		if (!("func" in types_heap[idx])) {
			print "function corrupted in " val
		} else {
			types_check(types_heap[idx]["func"])
		}
		types_check(types_heap[idx]["meta"])
		return
	case /^\$/:
		if (!("body" in types_heap[idx])) {
			print "function body corrupted in " val
		} else {
			types_check(types_heap[idx]["body"])
		}
		if (!("params" in types_heap[idx])) {
			print "function params corrupted in " val
		} else {
			types_check(types_heap[idx]["params"])
		}
		if (!("env" in types_heap[idx])) {
			print "function env corrupted in " val
		} else {
			env_check(types_heap[idx]["env"])
		}
		types_check(types_heap[idx]["meta"])
		return
	default:
		print "unknown type " val
		return
	}
}

function types_dump(i, j)
{
	for (i = 0; i < types_heap_index; i++) {
		if (i in types_heap) {
			if (isarray(types_heap[i])) {
				if (!("checked" in types_heap[i]) || types_heap[i]["checked"] != types_heap[i]["ref"]) {
					for (j in types_heap[i]) {
						print "  types_heap[" i "][" j "] = " types_heap[i][j]
					}
				}
			} else {
				print "  types_heap[" i "] = " types_heap[i]
			}
		}
	}
}

function types_typename(str)
{
	switch (str) {
	case /^"/:        return "string"
	case /^'/:        return "symbol"
	case /^:/:        return "keyword"
	case /^\+/:       return "number"
	case /^#nil$/:    return "nil"
	case /^#true$/:   return "true"
	case /^#false$/:  return "false"
	case /^\(/:       return "list"
	case /^\[/:       return "vector"
	case /^\{/:       return "hash"
	case /^\?/:       return "atom"
	case /^[&%]/:     return "builtin function"
	case /^\$/:       return "user defined function"
	}
}

BEGIN {
	types_heap_index = 0
}
