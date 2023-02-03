import mal
import readline { read_line }

type RepEnv = map[string]mal.Type

fn rep_read(input string) !mal.Type {
	return mal.read_str(input)!
}

fn eval(ast mal.Type, env RepEnv) !mal.Type {
	match ast {
		mal.List {
			if ast.list.len == 0 {
				return ast
			} else {
				// BUG: https://github.com/vlang/v/issues/17156
				// res := eval_ast(ast, env)! as mal.List
				res_tmp := eval_ast(ast, env)!
				res := res_tmp as mal.List
				fn_ := res.list[0].fn_()!
				return fn_(res.rest())
			}
		}
		else {
			return eval_ast(ast, env)!
		}
	}
	return ast
}

fn eval_ast(ast mal.Type, env RepEnv) !mal.Type {
	match ast {
		mal.Symbol {
			// return env[ ast.sym ] or { error( 'unknown: ${ast.sym}' ) }
			if res := env[ast.sym] {
				return res
			} else {
				return error('unknown: ${ast.sym}')
			}
		}
		mal.List {
			return mal.List{ast.list.map(eval(it, env)!)}
		}
		mal.Vector {
			return mal.Vector{ast.vec.map(eval(it, env)!)}
		}
		mal.Hashmap {
			mut hm := map[string]mal.Type{}
			for key in ast.hm.keys() {
				hm[key] = eval(ast.hm[key] or { panic('') }, env)!
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

fn rep(line string, env RepEnv) string {
	ast := rep_read(line) or {
		return if err.msg() == 'no form' { '' } else { 'READ ERROR: ${err}' }
	}
	$if ast ? {
		println('AST:\n${ast}')
	}
	res := eval(ast, env) or { return 'EVAL ERROR: ${err}' }
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
	env := RepEnv({
		'+': mal.Type(mal.Fn{fn (args mal.List) !mal.Type {
			a, b := get_args('+', args)!
			return mal.Int{a + b}
		}})
		'-': mal.Type(mal.Fn{fn (args mal.List) !mal.Type {
			a, b := get_args('-', args)!
			return mal.Int{a - b}
		}})
		'*': mal.Type(mal.Fn{fn (args mal.List) !mal.Type {
			a, b := get_args('*', args)!
			return mal.Int{a * b}
		}})
		'/': mal.Type(mal.Fn{fn (args mal.List) !mal.Type {
			a, b := get_args('/', args)!
			return mal.Int{a / b}
		}})
	})

	for {
		line := read_line('user> ') or {
			println('')
			break
		}
		out := rep(line, env)
		if out.len > 0 {
			println(out)
		}
	}
}
