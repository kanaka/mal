import mal
import readline { read_line }

type RepEnv = map[string]mal.MalType

fn rep_read(input string) !mal.MalType {
	return mal.read_str(input)!
}

fn rep_eval(ast mal.MalType, env RepEnv) !mal.MalType {
	match ast {
		mal.MalList {
			if ast.list.len == 0 {
				return ast
			} else {
				res := eval_ast(ast, env)! as mal.MalList
				res0 := res.list[0]
				if res0 is mal.MalFn {
					return res0.f(res.list[1..])
				} else {
					return error('bad func')
				}
			}
		}
		else {
			return eval_ast(ast, env)!
		}
	}
	return ast
}

fn eval_ast(ast mal.MalType, env RepEnv) !mal.MalType {
	match ast {
		mal.MalSymbol {
			// return env[ ast.sym ] or { error( 'unknown: ${ast.sym}' ) }
			if res := env[ast.sym] {
				return res
			} else {
				return error('unknown: ${ast.sym}')
			}
		}
		mal.MalList {
			return mal.MalList{ast.list.map(rep_eval(it, env)!)}
		}
		mal.MalVector {
			return mal.MalVector{ast.vec.map(rep_eval(it, env)!)}
		}
		mal.MalHashmap {
			mut hm := map[string]mal.MalType{}
			for key in ast.hm.keys() {
				hm[key] = rep_eval(ast.hm[key] or { panic('') }, env)!
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

fn rep(line string, env RepEnv) string {
	if ast := rep_read(line) {
		$if tokenise ? {
			println('AST:\n${ast}')
		}
		if res := rep_eval(ast, env) {
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
	env := RepEnv({
		'+': mal.MalType(mal.MalFn{fn (args []mal.MalType) !mal.MalType {
			a, b := get_args('+', args)!
			return mal.MalInt{a + b}
		}})
		'-': mal.MalType(mal.MalFn{fn (args []mal.MalType) !mal.MalType {
			a, b := get_args('-', args)!
			return mal.MalInt{a - b}
		}})
		'*': mal.MalType(mal.MalFn{fn (args []mal.MalType) !mal.MalType {
			a, b := get_args('*', args)!
			return mal.MalInt{a * b}
		}})
		'/': mal.MalType(mal.MalFn{fn (args []mal.MalType) !mal.MalType {
			a, b := get_args('/', args)!
			return mal.MalInt{a / b}
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
