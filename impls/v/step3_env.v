import mal
import readline { read_line }

fn rep_read(input string) !mal.MalType {
	return mal.read_str(input)!
}

fn rep_eval(ast mal.MalType, mut env mal.Env) !mal.MalType {
	match ast {
		mal.MalList {
			if ast.list.len < 1 {
				return ast
			}
			list0 := ast.list[0]
			special := if list0 is mal.MalSymbol { list0.sym } else { '' }
			match special {
				'def!' {
					if ast.list.len < 3 {
						return error('def!: missing param')
					}
					key := ast.list[1]
					if key is mal.MalSymbol {
						res := rep_eval(ast.list[2], mut env)!
						return env.set(key.sym, res)
					} else {
						return error('def!: bad symbol')
					}
				}
				'let*' {
					mut new_env := mal.mk_env(env)
					if ast.list.len < 2 {
						return error('let*: missing bindings')
					}
					bindings := ast.list[1]
					mut list := match bindings {
						mal.MalList { bindings.list }
						mal.MalVector { bindings.vec }
						else { return error('let*: bad bindings') }
					}
					list = list[0..] // shallow copy
					if list.len % 2 == 1 {
						return error('let*: extra binding param')
					}
					for list.len > 0 {
						key := list[0]
						if key is mal.MalSymbol {
							res := rep_eval(list[1], mut new_env)!
							new_env.set(key.sym, res)
						} else {
							return error('let*: bad binding symbol')
						}
						list = list[2..]
					}
					if ast.list.len > 2 {
						return rep_eval(ast.list[2], mut new_env)!
					} else {
						return error('let*: missing exp')
					}
				}
				else { // list apply
					// BUG: https://github.com/vlang/v/issues/17156
					// res := eval_ast(ast, env)! as mal.MalList
					res_tmp := eval_ast(ast, mut env)!
					res := res_tmp as mal.MalList
					res0 := res.list[0]
					if res0 is mal.MalFn {
						return res0.f(res.list[1..])
					} else {
						return error('bad func')
					}
				}
			}
		}
		else {
			return eval_ast(ast, mut env)!
		}
	}
	return ast
}

fn eval_ast(ast mal.MalType, mut env mal.Env) !mal.MalType {
	match ast {
		mal.MalSymbol {
			return env.get(ast.sym)!
		}
		mal.MalList {
			return mal.MalList{ast.list.map(rep_eval(it, mut env)!)}
		}
		mal.MalVector {
			return mal.MalVector{ast.vec.map(rep_eval(it, mut env)!)}
		}
		mal.MalHashmap {
			mut hm := map[string]mal.MalType{}
			for key in ast.hm.keys() {
				hm[key] = rep_eval(ast.hm[key] or { panic('') }, mut env)!
			}
			return mal.MalHashmap{hm}
		}
		else {
			return ast
		}
	}
}

fn rep_print(ast mal.MalType) string {
	return mal.pr_str(ast)
}

fn rep(line string, mut env mal.Env) string {
	if ast := rep_read(line) {
		$if tokenise ? {
			println('AST:\n${ast}')
		}
		if res := rep_eval(ast, mut env) {
			return rep_print(res)
		} else {
			return 'EVAL ERROR: ${err}'
		}
	} else {
		return if err.msg() == 'no form' { '' } else { 'READ ERROR: ${err}' }
	}
}

fn get_args(op string, args []mal.MalType) !(i64, i64) {
	if args.len != 2 {
		return error('${op}: takes 2 args')
	}
	arg0, arg1 := args[0], args[1]
	if arg0 is mal.MalInt {
		if arg1 is mal.MalInt {
			return arg0.val, arg1.val
		}
	}
	return error('${op}: int expected')
}

fn main() {
	mut env := mal.mk_outer_env()
	env.set('+', mal.MalType(mal.MalFn{fn (args []mal.MalType) !mal.MalType {
		a, b := get_args('+', args)!
		return mal.MalInt{a + b}
	}}))
	env.set('-', mal.MalType(mal.MalFn{fn (args []mal.MalType) !mal.MalType {
		a, b := get_args('-', args)!
		return mal.MalInt{a - b}
	}}))
	env.set('*', mal.MalType(mal.MalFn{fn (args []mal.MalType) !mal.MalType {
		a, b := get_args('*', args)!
		return mal.MalInt{a * b}
	}}))
	env.set('/', mal.MalType(mal.MalFn{fn (args []mal.MalType) !mal.MalType {
		a, b := get_args('/', args)!
		return mal.MalInt{a / b}
	}}))

	for {
		line := read_line('user> ') or {
			println('')
			break
		}
		out := rep(line, mut env)
		if out.len > 0 {
			println(out)
		}
	}
}
