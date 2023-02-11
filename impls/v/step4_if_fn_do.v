import mal
import readline { read_line }

fn rep_read(input string) !mal.Type {
	return mal.read_str(input)!
}

fn eval(ast mal.Type, mut env mal.Env) !mal.Type {
	if ast is mal.List {
		first := ast.first() or { return ast } // return empty list
		args := ast.rest()
		match first.sym() or { '' } {
			'def!' {
				mal.check_args(args, 2, 2) or { return error('def!: ${err}') }
				sym := args.nth(0).sym() or { return error('def!: ${err}') }
				return env.set(sym, eval(args.nth(1), mut env)!)
			}
			'let*' {
				mal.check_args(args, 2, 2) or { return error('let*: ${err}') }
				mut new_env := mal.mk_env(&env)
				tmp := args.nth(0).sequence() or { return error('let*: ${err}') }
				mut pairs := tmp[0..] // copy
				if pairs.len % 2 == 1 {
					return error('let*: extra binding param')
				}
				for pairs.len > 0 {
					sym := pairs[0].sym() or { return error('let*: ${err}') }
					new_env.set(sym, eval(pairs[1], mut new_env)!)
					pairs = pairs[2..]
				}
				return eval(args.nth(1), mut new_env)!
			}
			'do' {
				res := eval_ast(args, mut env)! as mal.List
				return *res.last()!
			}
			'if' {
				mal.check_args(args, 2, 3) or { return error('if: ${err}') }
				res := eval(args.nth(0), mut env)!
				clause := args.nth(if res.truthy() { 1 } else { 2 })
				return eval(clause, mut env)!
			}
			'fn*' {
				mal.check_args(args, 2, 2) or { return error('fn*: ${err}') }
				binds := args.nth(0).sequence() or { return error('fn*: ${err}') }
				syms := binds.map(it.sym() or { return error('fn*: ${err}') })
				body := args.nth(1)
				for i, sym in syms {
					if sym == '&' && syms.len != i + 2 {
						return error('fn*: & has 1 arg')
					}
				}
				closure := fn [env, syms, body] (args mal.List) !mal.Type {
					mut new_env := mal.mk_env(&env)
					for i, sym in syms {
						if sym == '&' {
							new_env.set(syms[i + 1], args.from(i))
							break
						} else {
							new_env.set(sym, args.nth(i))
						}
					}
					return eval(body, mut new_env)!
				}
				return mal.new_fn(closure)
			}
			else { // regular list apply
				res := eval_ast(ast, mut env)! as mal.List
				f := res.list[0].fn_()!
				return f(res.rest())
			}
		}
	} else {
		return eval_ast(ast, mut env)!
	}
}

fn eval_ast(ast mal.Type, mut env mal.Env) !mal.Type {
	match ast {
		mal.Symbol {
			return env.get(ast.sym)!
		}
		mal.List {
			return mal.new_list(ast.list.map(eval(it, mut env)!))
		}
		mal.Vector {
			return mal.Vector{
				vec: ast.vec.map(eval(it, mut env)!)
			}
		}
		mal.Hashmap {
			mut hm := map[string]mal.Type{}
			for key in ast.hm.keys() {
				hm[key] = eval(ast.hm[key] or { panic('') }, mut env)!
			}
			return mal.new_hashmap(hm)
		}
		else {
			return ast
		}
	}
}

fn rep_print(ast mal.Type) string {
	return mal.pr_str(ast, true)
}

fn rep(line string, mut env mal.Env) string {
	ast := rep_read(line) or {
		return if err.msg() == 'no form' { '' } else { 'READ ERROR: ${err}' }
	}
	$if ast ? {
		println('AST:\n${ast}')
	}
	res := eval(ast, mut env) or { return 'EVAL ERROR: ${err}' }
	return rep_print(res)
}

fn main() {
	// outer-most env
	mut env := mal.Env{}

	// core env
	mal.add_core(mut env, eval)

	// mal defined env
	rep('(def! not (fn* (a) (if a false true)))', mut env)

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
