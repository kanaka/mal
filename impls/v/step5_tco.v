import mal
import readline { read_line }

fn rep_read(input string) !mal.Type {
	return mal.read_str(input)!
}

fn eval(ast_ mal.Type, mut env_ mal.Env) !mal.Type {
	mut ast := unsafe { &ast_ }
	mut env := unsafe { &env_ }
	for true {
		if ast is mal.List {
			node := ast as mal.List
			first := node.first() or { return *ast } // return empty list
			args := node.rest()
			match first.sym() or { '' } {
				'def!' {
					mal.check_args(args, 2, 2) or { return error('def!: ${err}') }
					sym := args.nth(0).sym() or { return error('def!: ${err}') }
					return env.set(sym, eval(args.nth(1), mut env)!)
				}
				'let*' {
					mal.check_args(args, 2, 2) or { return error('let*: ${err}') }
					env = unsafe { mal.mk_env(env) } // TCO
					tmp := args.nth(0).list_or_vec() or { return error('let*: ${err}') }
					mut pairs := tmp[0..] // copy
					if pairs.len % 2 == 1 {
						return error('let*: extra binding param')
					}
					for pairs.len > 0 {
						sym := pairs[0].sym() or { return error('let*: ${err}') }
						env.set(sym, eval(pairs[1], mut env)!)
						pairs = pairs[2..]
					}
					ast = unsafe { args.nth(1) } // TCO
				}
				'do' {
					if args.len() > 1 {
						args.list[0..args.list.len - 1].map(eval(it, mut env)!)
					} else if args.len() == 0 {
						return mal.Nil{}
					}
					ast = unsafe { args.last()! } // TCO
				}
				'if' {
					mal.check_args(args, 2, 3) or { return error('if: ${err}') }
					if eval(args.nth(0), mut env)!.truthy() {
						ast = unsafe { args.nth(1) } // TCO
					} else if args.len() == 3 {
						ast = unsafe { args.nth(2) } // TCO
					} else {
						return mal.Nil{}
					}
				}
				'fn*' {
					mal.check_args(args, 2, 2) or { return error('fn*: ${err}') }
					binds := args.nth(0).list_or_vec() or { return error('fn*: ${err}') }
					syms := binds.map(it.sym() or { return error('fn*: ${err}') })
					for i, sym in syms {
						if sym == '&' && syms.len != i + 2 {
							return error('fn*: & has 1 arg')
						}
					}
					return mal.Closure{args.nth(1), syms, env}
				}
				else { // regular list apply
					res := eval_ast(ast, mut env)! as mal.List
					list0 := res.list[0]
					if list0 is mal.Fn {
						return list0.f(res.rest())!
					} else if list0 is mal.Closure {
						ast = &list0.ast // TCO
						env = mal.mk_env(list0.env)
						env.bind(list0.params, res.rest())
					} else {
						return error('function expected')
					}
				}
			}
		} else {
			return eval_ast(ast, mut env)!
		}
	}
	panic('unreachable code')
}

fn eval_ast(ast mal.Type, mut env mal.Env) !mal.Type {
	match ast {
		mal.Symbol {
			return env.get(ast.sym)!
		}
		mal.List {
			return mal.List{ast.list.map(eval(it, mut env)!)}
		}
		mal.Vector {
			return mal.Vector{ast.vec.map(eval(it, mut env)!)}
		}
		mal.Hashmap {
			mut hm := map[string]mal.Type{}
			for key in ast.hm.keys() {
				hm[key] = eval(ast.hm[key] or { panic('') }, mut env)!
			}
			return mal.Hashmap{hm}
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
