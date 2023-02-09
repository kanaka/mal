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

fn get_args(op string, args mal.List) !(i64, i64) {
	if args.len() != 2 {
		return error('${op}: takes 2 args')
	}
	arg0, arg1 := args.list[0], args.list[1]
	if arg0 is mal.Int {
		if arg1 is mal.Int {
			return arg0.val, arg1.val
		}
	}
	return error('${op}: int expected')
}

fn main() {
	mut env := mal.Env{}
	env.set('+', mal.Type(mal.Fn{fn (args mal.List) !mal.Type {
		a, b := get_args('+', args)!
		return mal.Int{a + b}
	}}))
	env.set('-', mal.Type(mal.Fn{fn (args mal.List) !mal.Type {
		a, b := get_args('-', args)!
		return mal.Int{a - b}
	}}))
	env.set('*', mal.Type(mal.Fn{fn (args mal.List) !mal.Type {
		a, b := get_args('*', args)!
		return mal.Int{a * b}
	}}))
	env.set('/', mal.Type(mal.Fn{fn (args mal.List) !mal.Type {
		a, b := get_args('/', args)!
		return mal.Int{a / b}
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
