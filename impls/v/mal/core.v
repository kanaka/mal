module mal

import os
import time
import readline { read_line }

type EvalFn = fn (_ Type, mut _ Env) !Type

pub fn apply(ast Type, eval_fn EvalFn, args List) !Type {
	if ast is Fn {
		return ast.f(args)!
	} else if ast is Closure {
		mut env := mk_env(ast.env)
		env.bind(ast.params, args)
		return eval_fn(ast.ast, mut env)!
	} else {
		return error('function expected')
	}
}

pub fn check_args(args List, min int, max int) ! {
	if args.len() < min {
		return error('missing param')
	} else if max >= 0 && args.len() > max {
		return error('extra param')
	}
}

fn wrap_err(sym string, err IError) IError {
	return if err is Exception { IError(err) } else { error('${sym}: ${err}') }
}

pub fn add_fn(mut env Env, sym string, min int, max int, f FnDef) {
	env.set(sym, new_fn(fn [sym, min, max, f] (args List) !Type {
		check_args(args, min, max) or { return wrap_err(sym, err) }
		return f(args) or { return wrap_err(sym, err) }
	}))
}

pub fn add_core(mut env Env, eval_fn EvalFn) {
	add_fn(mut env, '+', 2, 2, fn (args List) !Type {
		return Int{args.nth(0).int_()! + args.nth(1).int_()!}
	})
	add_fn(mut env, '-', 2, 2, fn (args List) !Type {
		return Int{args.nth(0).int_()! - args.nth(1).int_()!}
	})
	add_fn(mut env, '*', 2, 2, fn (args List) !Type {
		return Int{args.nth(0).int_()! * args.nth(1).int_()!}
	})
	add_fn(mut env, '/', 2, 2, fn (args List) !Type {
		return Int{args.nth(0).int_()! / args.nth(1).int_()!}
	})
	add_fn(mut env, 'list', -1, -1, fn (args List) !Type {
		return args
	})
	add_fn(mut env, 'list?', 1, 1, fn (args List) !Type {
		return make_bool(args.nth(0) is List)
	})
	add_fn(mut env, 'empty?', 1, 1, fn (args List) !Type {
		return make_bool(args.nth(0).sequence()!.len == 0)
	})
	add_fn(mut env, 'count', 1, 1, fn (args List) !Type {
		return Int{args.nth(0).sequence()!.len}
	})
	add_fn(mut env, '=', 2, 2, fn (args List) !Type {
		return make_bool(args.nth(0).eq(args.nth(1)))
	})
	add_fn(mut env, '<', 2, 2, fn (args List) !Type {
		return make_bool(args.nth(0).lt(args.nth(1))!)
	})
	add_fn(mut env, '<=', 2, 2, fn (args List) !Type {
		return make_bool(args.nth(0).lt(args.nth(1))! || args.nth(0).eq(args.nth(1)))
	})
	add_fn(mut env, '>', 2, 2, fn (args List) !Type {
		return make_bool(!args.nth(0).lt(args.nth(1))! && !args.nth(0).eq(args.nth(1)))
	})
	add_fn(mut env, '>=', 2, 2, fn (args List) !Type {
		return make_bool(!args.nth(0).lt(args.nth(1))!)
	})
	add_fn(mut env, 'pr-str', -1, -1, fn (args List) !Type {
		return String{args.list.map(pr_str(it, true)).join(' ')}
	})
	add_fn(mut env, 'str', -1, -1, fn (args List) !Type {
		return String{args.list.map(pr_str(it, false)).join('')}
	})
	add_fn(mut env, 'prn', -1, -1, fn (args List) !Type {
		println(args.list.map(pr_str(it, true)).join(' '))
		return Nil{}
	})
	add_fn(mut env, 'println', -1, -1, fn (args List) !Type {
		println(args.list.map(pr_str(it, false)).join(' '))
		return Nil{}
	})
	add_fn(mut env, 'read-string', 1, 1, fn (args List) !Type {
		return read_str(args.nth(0).str_()!)!
	})
	add_fn(mut env, 'slurp', 1, 1, fn (args List) !Type {
		return String{os.read_file(args.nth(0).str_()!)!}
	})
	add_fn(mut env, 'atom', 1, 1, fn (args List) !Type {
		return Atom{args.nth(0)}
	})
	add_fn(mut env, 'atom?', 1, 1, fn (args List) !Type {
		return make_bool(args.nth(0) is Atom)
	})
	add_fn(mut env, 'deref', 1, 1, fn (args List) !Type {
		return args.nth(0).atom()!.typ
	})
	add_fn(mut env, 'reset!', 2, 2, fn (args List) !Type {
		return args.nth(0).atom()!.set(args.nth(1))
	})
	add_fn(mut env, 'swap!', 2, -1, fn [eval_fn] (args List) !Type {
		atom := args.nth(0).atom()!
		// BUG: << doesn't like templated sumtype array args
		// https://github.com/vlang/v/issues/17259
		// list := arrays.concat[Type]([atom.typ], ...args.from(2).list)
		mut list := [atom.typ]
		list << args.from(2).list
		return atom.set(apply(args.nth(1), eval_fn, new_list(list))!)
	})
	add_fn(mut env, 'cons', 2, 2, fn (args List) !Type {
		// BUG: << doesn't like templated sumtype array args
		// https://github.com/vlang/v/issues/17259
		// return new_list(arrays.concat[Type]([*args.nth(0)], ...args.nth(1).list()!))
		mut list := [*args.nth(0)]
		list << args.nth(1).sequence()!
		return new_list(list)
	})
	add_fn(mut env, 'concat', 0, -1, fn (args List) !Type {
		mut list := []Type{}
		for arg in args.list {
			list << arg.sequence()!
		}
		return new_list(list)
	})
	add_fn(mut env, 'vec', 1, 1, fn (args List) !Type {
		return new_vector(args.nth(0).sequence()!)
	})
	add_fn(mut env, 'nth', 2, 2, fn (args List) !Type {
		list := args.nth(0).sequence()!
		i := args.nth(1).int_()!
		return if i < list.len { list[i] } else { error('out of range') }
	})
	add_fn(mut env, 'first', 1, 1, fn (args List) !Type {
		list := new_list(args.nth(0).sequence()!)
		return *list.nth(0)
	})
	add_fn(mut env, 'rest', 1, 1, fn (args List) !Type {
		list := new_list(args.nth(0).sequence()!)
		return list.rest()
	})
	add_fn(mut env, 'throw', 1, 1, fn (args List) !Type {
		return IError(Exception{Error{}, args.nth(0)})
	})
	add_fn(mut env, 'apply', 2, -1, fn [eval_fn] (args List) !Type {
		// BUG: << doesn't like templated sumtype array args
		// https://github.com/vlang/v/issues/17259
		// list := arrays.concat(args.range(1, args.len() - 2).list, args.last()!.sequence()!)
		mut list := args.list[1..args.len() - 1]
		list << args.last()!.sequence()!
		return apply(args.nth(0), eval_fn, new_list(list))
	})
	add_fn(mut env, 'map', 2, 2, fn [eval_fn] (args List) !Type {
		mut list := []Type{}
		for typ in args.nth(1).sequence()! {
			list << apply(args.nth(0), eval_fn, new_list([typ]))!
		}
		return new_list(list)
	})
	add_fn(mut env, 'nil?', 1, 1, fn (args List) !Type {
		return make_bool(args.nth(0) is Nil)
	})
	add_fn(mut env, 'true?', 1, 1, fn (args List) !Type {
		return make_bool(args.nth(0) is True)
	})
	add_fn(mut env, 'false?', 1, 1, fn (args List) !Type {
		return make_bool(args.nth(0) is False)
	})
	add_fn(mut env, 'symbol?', 1, 1, fn (args List) !Type {
		return make_bool(args.nth(0) is Symbol)
	})
	add_fn(mut env, 'symbol', 1, 1, fn (args List) !Type {
		return Symbol{args.nth(0).str_()!}
	})
	add_fn(mut env, 'keyword', 1, 1, fn (args List) !Type {
		arg0 := args.nth(0)
		return match arg0 {
			Keyword { arg0 }
			String { Keyword{arg0.val} }
			else { error('keyword/string expected') }
		}
	})
	add_fn(mut env, 'keyword?', 1, 1, fn (args List) !Type {
		return make_bool(args.nth(0) is Keyword)
	})
	add_fn(mut env, 'vector', -1, -1, fn (args List) !Type {
		return new_vector(args.list)
	})
	add_fn(mut env, 'vector?', 1, 1, fn (args List) !Type {
		return make_bool(args.nth(0) is Vector)
	})
	add_fn(mut env, 'sequential?', 1, 1, fn (args List) !Type {
		return make_bool(args.nth(0) in [Vector, List])
	})
	add_fn(mut env, 'hash-map', -1, -1, fn (args List) !Type {
		return new_hashmap({}).load(args)!
	})
	add_fn(mut env, 'map?', 1, 1, fn (args List) !Type {
		return make_bool(args.nth(0) is Hashmap)
	})
	add_fn(mut env, 'assoc', 2, -1, fn (args List) !Type {
		return new_hashmap(args.first()!.hashmap()!.hm).load(args.rest())!
	})
	add_fn(mut env, 'dissoc', 2, -1, fn (args List) !Type {
		return args.first()!.hashmap()!.filter(args.from(1))!
	})
	add_fn(mut env, 'get', 2, 2, fn (args List) !Type {
		return args.nth(0).hashmap()!.get(args.nth(1).key()!)
	})
	add_fn(mut env, 'contains?', 2, 2, fn (args List) !Type {
		return make_bool(args.nth(0).hashmap()!.has(args.nth(1).key()!))
	})
	add_fn(mut env, 'keys', 1, 1, fn (args List) !Type {
		return new_list(args.nth(0).hashmap()!.hm.keys().map(unkey(it)))
	})
	add_fn(mut env, 'vals', 1, 1, fn (args List) !Type {
		return new_list(args.nth(0).hashmap()!.hm.values())
	})
	add_fn(mut env, 'readline', 1, 1, fn (args List) !Type {
		if line := read_line(args.nth(0).str_()!) {
			return String{line.replace('\n', '')}
		} else {
			println('') // newline
			return Nil{}
		}
	})
	add_fn(mut env, 'time-ms', -1, -1, fn (args List) !Type {
		return Int{time.ticks()}
	})
	add_fn(mut env, 'meta', 1, 1, fn (args List) !Type {
		return args.nth(0).get_meta()
	})
	add_fn(mut env, 'with-meta', 2, 2, fn (args List) !Type {
		return args.nth(0).set_meta(args.nth(1))!
	})
	add_fn(mut env, 'fn?', 1, 1, fn (args List) !Type {
		f := args.nth(0)
		return make_bool(match f {
			Fn { true }
			Closure { !f.is_macro }
			else { false }
		})
	})
	add_fn(mut env, 'string?', 1, 1, fn (args List) !Type {
		return make_bool(args.nth(0) is String)
	})
	add_fn(mut env, 'number?', 1, 1, fn (args List) !Type {
		return make_bool(args.nth(0) in [Int, Float])
	})
	add_fn(mut env, 'seq', 1, 1, fn (args List) !Type {
		return match args.nth(0) {
			List, Vector {
				if args.nth(0).sequence()!.len == 0 {
					Nil{}
				} else {
					new_list(args.nth(0).sequence()!)
				}
			}
			String {
				if args.nth(0).str_()! == '' {
					Nil{}
				} else {
					new_list(args.nth(0).str_()!.split('').map(Type(String{it})))
				}
			}
			else {
				Nil{}
			}
		}
	})
	add_fn(mut env, 'conj', 2, -1, fn (args List) !Type {
		t := args.nth(0)
		return match t {
			// https://github.com/vlang/v/issues/17333
			// List, Vector { t.conj(args.from(1)) }
			List { t.conj(args.from(1)) }
			Vector { t.conj(args.from(1)) }
			else { error('vector/list expected') }
		}
	})
	add_fn(mut env, 'macro?', 1, 1, fn (args List) !Type {
		f := args.nth(0)
		return make_bool(if f is Closure { f.is_macro } else { false })
	})
}
