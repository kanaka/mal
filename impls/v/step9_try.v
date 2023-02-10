import mal
import os
import readline { read_line }

fn rep_read(input string) !mal.Type {
	return mal.read_str(input)!
}

fn quasiquote_list(list []mal.Type) !mal.Type {
	mut res := []mal.Type{}
	for elt in list.reverse() {
		if elt is mal.List && elt.call_sym() or { '' } == 'splice-unquote' {
			res = [mal.Symbol{'concat'}, elt.nth(1), mal.List{res}]
		} else {
			res = [mal.Symbol{'cons'}, quasiquote(elt)!, mal.List{res}]
		}
	}
	return mal.List{res}
}

fn quasiquote(ast mal.Type) !mal.Type {
	return match ast {
		mal.List {
			if ast.nth(0).eq(mal.Symbol{'unquote'}) {
				*ast.nth(1)
			} else {
				quasiquote_list(ast.list)!
			}
		}
		mal.Vector {
			mal.List{[mal.Symbol{'vec'}, quasiquote_list(ast.vec)!]}
		}
		mal.Symbol, mal.Hashmap {
			mal.List{[mal.Symbol{'quote'}, ast]}
		}
		else {
			ast
		}
	}
}

fn is_macro_call(ast mal.Type, env mal.Env) ?mal.Closure {
	if sym := ast.call_sym() {
		if cls := env.find(sym) {
			if cls is mal.Closure {
				if cls.is_macro {
					return cls
				}
			}
		}
	}
	return none
}

fn macroexpand(ast_ mal.Type, env mal.Env) !mal.Type {
	mut ast := unsafe { &ast_ }
	for {
		cls := is_macro_call(ast, env) or { break }
		res := mal.apply(cls, eval, (ast as mal.List).rest())!
		ast = unsafe { &res }
	}
	return *ast
}

fn eval(ast_ mal.Type, mut env_ mal.Env) !mal.Type {
	mut ast := unsafe { &ast_ }
	mut env := unsafe { &env_ }
	for true {
		if ast !is mal.List {
			return eval_ast(ast, mut env)!
		} else if (ast as mal.List).len() == 0 {
			return *ast
		}

		// macro expansion
		expanded := macroexpand(ast, env)!
		ast = unsafe { &expanded }
		if ast !is mal.List {
			return eval_ast(ast, mut env)!
		} else if (ast as mal.List).len() == 0 {
			return *ast
		}

		ast0 := (ast as mal.List).first()!
		args := (ast as mal.List).rest()
		match ast0.sym() or { '' } {
			'def!' {
				mal.check_args(args, 2, 2) or { return error('def!: ${err}') }
				sym := args.nth(0).sym() or { return error('def!: ${err}') }
				return env.set(sym, eval(args.nth(1), mut env)!)
			}
			'let*' {
				mal.check_args(args, 2, 2) or { return error('let*: ${err}') }
				env = unsafe { mal.mk_env(env) } // TCO
				tmp := args.nth(0).sequence() or { return error('let*: ${err}') }
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
				binds := args.nth(0).sequence() or { return error('fn*: ${err}') }
				syms := binds.map(it.sym() or { return error('fn*: ${err}') })
				for i, sym in syms {
					if sym == '&' && syms.len != i + 2 {
						return error('fn*: & has 1 arg')
					}
				}
				return mal.Closure{args.nth(1), syms, env, false}
			}
			'quote' {
				mal.check_args(args, 1, 1) or { return error('quote: ${err}') }
				return *args.nth(0)
			}
			'quasiquoteexpand' {
				mal.check_args(args, 1, 1) or { return error('quasiquoteexpand: ${err}') }
				return quasiquote(args.nth(0)) or { return error('quasiquoteexpand: ${err}') }
			}
			'quasiquote' {
				mal.check_args(args, 1, 1) or { return error('quasiquote: ${err}') }
				res := quasiquote(args.nth(0)) or { return error('quasiquote: ${err}') }
				ast = unsafe { &res } // TCO
			}
			'defmacro!' {
				mal.check_args(args, 2, 2) or { return error('defmacro!: ${err}') }
				sym := args.nth(0).sym() or { return error('defmacro!: ${err}') }
				cls := eval(args.nth(1), mut env)!.cls() or { return error('defmacro!: ${err}') }
				return env.set(sym, cls.to_macro())
			}
			'macroexpand' {
				mal.check_args(args, 1, 1) or { return error('macroexpand: ${err}') }
				return macroexpand(args.nth(0), env)
			}
			'try*' {
				mal.check_args(args, 1, 2) or { return error('try*: ${err}') }
				if res := eval(args.nth(0), mut env) {
					return res
				} else {
					if args.nth(1).call_sym() or { '' } == 'catch*' {
						cargs := (args.nth(1) as mal.List).rest()
						mal.check_args(cargs, 2, 2) or { return error('catch* ${err}') }
						sym := cargs.nth(0).sym() or { return error('catch*: ${err}') }
						typ := if err is mal.Exception { err.typ } else { mal.String{err.msg()} }
						ast = cargs.nth(1) // TCO
						env = unsafe { mal.mk_env(env) }
						env.set(sym, typ)
					} else {
						return err
					}
				}
			}
			else { // regular list apply w/ TCO
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
		if err.msg() != 'no form' {
			println('Error: ${err}')
		}
		return ''
	}
	$if ast ? {
		println('AST:\n${ast}')
	}
	if res := eval(ast, mut env) {
		return rep_print(res)
	} else {
		if err is mal.Exception {
			println('Exception: ${mal.pr_str(err.typ, true)}')
		} else {
			println('Error: ${err}')
		}
		return ''
	}
}

fn main() {
	// outer-most env
	mut env := mal.Env{}

	// add eval
	mut envp := &env // workaround no closure references in V
	mal.add_fn(mut env, 'eval', 1, 1, fn [mut envp] (args mal.List) !mal.Type {
		return eval(args.nth(0), mut envp)!
	})

	// core env
	mal.add_core(mut env, eval)

	// mal defined env
	rep('(def! not (fn* (a) (if a false true)))', mut env)
	rep('(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))', mut
		env)
	rep('(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list \'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons \'cond (rest (rest xs)))))))', mut
		env)

	if os.args.len > 1 {
		// run script
		file := os.args[1]
			.replace('\\', '\\\\')
			.replace('"', '\\"')
		mut args := []mal.Type{}
		for i in 2 .. os.args.len {
			args << mal.Type(mal.String{os.args[i]})
		}
		env.set('*ARGV*', mal.List{args})
		rep('(load-file "${file}")', mut env)
	} else {
		// repl
		env.set('*ARGV*', mal.List{})
		for {
			line := read_line('user> ') or {
				println('') // newline
				break
			}
			out := rep(line, mut env)
			if out.len > 0 {
				println(out)
			}
		}
	}
}
