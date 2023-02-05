import mal
import readline { read_line }

fn rep_read(input string) !mal.Type {
	return mal.read_str(input)!
}

fn eval(ast mal.Type, mut env mal.Env) !mal.Type {
	match ast {
		mal.List {
			first := ast.first() or { return ast } // return empty list
			match first.sym() or { '' } {
				'def!' {
					ast.nth(2) or { return error('def!: missing param') }
					sym := ast.list[1].sym() or { return error('def!: ${err}') }
					return env.set(sym, eval(ast.list[2], mut env)!)
				}
				'let*' {
					ast.nth(2) or { return error('let*: missing param') }
					mut new_env := mal.mk_env(&env)
					tmp := ast.list[1].list_or_vec() or { return error('let*: ${err}') }
					mut pairs := tmp[0..] // copy
					if pairs.len % 2 == 1 {
						return error('let*: extra binding param')
					}
					for pairs.len > 0 {
						sym := pairs[0].sym() or { return error('let*: ${err}') }
						new_env.set(sym, eval(pairs[1], mut new_env)!)
						pairs = pairs[2..]
					}
					return eval(ast.list[2], mut new_env)!
				}
				'do' {
					// BUG: https://github.com/vlang/v/issues/17156
					// res := eval_ast(ast.rest(), mut env)! as mal.List
					res_tmp := eval_ast(ast.rest(), mut env)!
					res := res_tmp as mal.List
					return res.last() or { mal.Nil{} }
				}
				'if' {
					ast.nth(2) or { return error('if: missing param') }
					res := eval(ast.list[1], mut env)!
					clause_n := if res.truthy() { 2 } else { 3 }
					clause := ast.nth(clause_n) or { return mal.Nil{} }
					return eval(clause, mut env)!
				}
				'fn*' {
					ast.nth(2) or { return error('fn*: missing param') }
					binds := ast.list[1].list_or_vec() or { return error('fn*: ${err}') }
					syms := binds.map(it.sym() or { return error('fn*: ${err}') })
					body := ast.list[2]
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
								new_env.set(sym, args.nth(i) or { mal.Nil{} })
							}
						}
						return eval(body, mut new_env)!
					}
					return mal.Fn{closure}
				}
				else { // regular list apply
					// BUG: https://github.com/vlang/v/issues/17156
					// res := eval_ast(ast, env)! as mal.List
					res_tmp := eval_ast(ast, mut env)!
					res := res_tmp as mal.List
					fn_ := res.list[0].fn_()!
					return fn_(res.rest())
				}
			}
		}
		else {
			return eval_ast(ast, mut env)!
		}
	}
	return ast
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
	for nsfn in mal.get_core() {
		env.set(nsfn.sym, mal.Fn{nsfn.fn_})
	}

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
