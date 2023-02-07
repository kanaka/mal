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
}
