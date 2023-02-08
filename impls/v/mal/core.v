module mal

import os

pub fn check_args(args List, min int, max int) ! {
	if args.len() < min {
		return error('missing param')
	} else if max >= 0 && args.len() > max {
		return error('extra param')
	}
}

pub fn add_fn(mut env Env, sym string, min int, max int, f FnDef) {
	env.set(sym, Fn{fn [sym, min, max, f] (args List) !Type {
		check_args(args, min, max) or { return error('${sym}: ${err}') }
		return f(args) or { error('${sym}: ${err}') }
	}})
}

type EvalFn = fn (Type, mut Env) !Type

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
		return make_bool(args.nth(0).list_or_vec()!.len == 0)
	})
	add_fn(mut env, 'count', 1, 1, fn (args List) !Type {
		return Int{args.nth(0).list_or_vec()!.len}
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
		return atom.set(args.nth(1).fn_apply(eval_fn, List{list})!)
	})
	add_fn(mut env, 'cons', 2, 2, fn (args List) !Type {
		// BUG: << doesn't like templated sumtype array args
		// https://github.com/vlang/v/issues/17259
		// return List{arrays.concat[Type]([*args.nth(0)], ...args.nth(1).list()!)}
		mut list := [*args.nth(0)]
		list << args.nth(1).list_or_vec()!
		return List{list}
	})
	add_fn(mut env, 'concat', 0, -1, fn (args List) !Type {
		mut list := []Type{}
		for arg in args.list {
			list << arg.list_or_vec()!
		}
		return List{list}
	})
	add_fn(mut env, 'vec', 1, 1, fn (args List) !Type {
		return Vector{args.nth(0).list_or_vec()!}
	})
	add_fn(mut env, 'nth', 2, 2, fn (args List) !Type {
		list := args.nth(0).list_or_vec()!
		i := args.nth(1).int_()!
		return if i < list.len { list[i] } else { error('out of range') }
	})
	add_fn(mut env, 'first', 1, 1, fn (args List) !Type {
		list := List{args.nth(0).list_or_vec()!}
		return *list.nth(0)
	})
	add_fn(mut env, 'rest', 1, 1, fn (args List) !Type {
		list := List{args.nth(0).list_or_vec()!}
		return list.rest()
	})
}
