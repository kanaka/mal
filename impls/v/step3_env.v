import mal
import readline { read_line }

fn rep_read(input string) !mal.MalType {
	return mal.read_str(input)!
}

fn eval(ast mal.MalType, mut env mal.Env) !mal.MalType {
	match ast {
		mal.MalList {
			first := ast.first() or { return ast } // return empty list
			match first.sym() or { '' } {
				'def!' {
					ast.nth(2) or { return error('def!: missing param') }
					sym := ast.list[1].sym() or { return error('def!: ${err}') }
					return env.set(sym, eval(ast.list[2], mut env)!)
				}
				'let*' {
					ast.nth(2) or { return error('let*: missing param') }
					mut new_env := mal.mk_env(env)
					mut pairs := ast.list[1].list_or_vec() or { return error('let*: ${err}') }
					pairs = pairs[0..] // copy
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
				else { // regular list apply
					// BUG: https://github.com/vlang/v/issues/17156
					// res := eval_ast(ast, env)! as mal.MalList
					res_tmp := eval_ast(ast, mut env)!
					res := res_tmp as mal.MalList
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

fn eval_ast(ast mal.MalType, mut env mal.Env) !mal.MalType {
	match ast {
		mal.MalSymbol {
			return env.get(ast.sym)!
		}
		mal.MalList {
			return mal.MalList{ast.list.map(eval(it, mut env)!)}
		}
		mal.MalVector {
			return mal.MalVector{ast.vec.map(eval(it, mut env)!)}
		}
		mal.MalHashmap {
			mut hm := map[string]mal.MalType{}
			for key in ast.hm.keys() {
				hm[key] = eval(ast.hm[key] or { panic('') }, mut env)!
			}
			return mal.MalHashmap{hm}
		}
		else {
			return ast
		}
	}
}

fn rep_print(ast mal.MalType) string {
	return mal.pr_str(ast, true)
}

fn rep(line string, mut env mal.Env) string {
    ast := rep_read(line) or {
		return if err.msg() == 'no form' { '' } else { 'READ ERROR: ${err}' }
    }
	$if tokenise ? {
		println('AST:\n${ast}')
	}
    res := eval(ast, mut env) or { return 'EVAL ERROR: ${err}' }
    return rep_print(res)
}

fn get_args(op string, args mal.MalList) !(i64, i64) {
	if args.len() != 2 {
		return error('${op}: takes 2 args')
	}
	arg0, arg1 := args.list[0], args.list[1]
	if arg0 is mal.MalInt {
		if arg1 is mal.MalInt {
			return arg0.val, arg1.val
		}
	}
	return error('${op}: int expected')
}

fn main() {
	mut env := mal.Env{}
	env.set('+', mal.MalType(mal.MalFn{fn (args mal.MalList) !mal.MalType {
		a, b := get_args('+', args)!
		return mal.MalInt{a + b}
	}}))
	env.set('-', mal.MalType(mal.MalFn{fn (args mal.MalList) !mal.MalType {
		a, b := get_args('-', args)!
		return mal.MalInt{a - b}
	}}))
	env.set('*', mal.MalType(mal.MalFn{fn (args mal.MalList) !mal.MalType {
		a, b := get_args('*', args)!
		return mal.MalInt{a * b}
	}}))
	env.set('/', mal.MalType(mal.MalFn{fn (args mal.MalList) !mal.MalType {
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
