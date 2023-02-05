module mal

struct NSFn {
pub:
	sym string
	fn_ FnFn
}

fn get_args(op string, args List) !(i64, i64) {
	args.nth(1) or { return error('${op}: takes 2 args') }
	a := args.list[0].int_() or { return error('${op}: ${err}') }
	b := args.list[1].int_() or { return error('${op}: ${err}') }
	return a, b
}

pub fn get_core() []NSFn {
	return [
		NSFn{'+', fn (args List) !Type {
			a, b := get_args('+', args)!
			return Int{a + b}
		}},
		NSFn{'-', fn (args List) !Type {
			a, b := get_args('-', args)!
			return Int{a - b}
		}},
		NSFn{'*', fn (args List) !Type {
			a, b := get_args('*', args)!
			return Int{a * b}
		}},
		NSFn{'/', fn (args List) !Type {
			a, b := get_args('/', args)!
			return Int{a / b}
		}},
		NSFn{'list', fn (args List) !Type {
			return args
		}},
		NSFn{'list?', fn (args List) !Type {
			first := args.nth(0) or { return False{} }
			return if first is List { True{} } else { False{} }
		}},
		NSFn{'empty?', fn (args List) !Type {
			first := args.nth(0) or { return error('empty?: missing param') }
			seq := first.list_or_vec() or { return error('empty?: ${err}') }
			return if seq.len == 0 { True{} } else { False{} }
		}},
		NSFn{'count', fn (args List) !Type {
			first := args.nth(0) or { return error('count: missing param') }
			seq := first.list_or_vec() or { return error('count: ${err}') }
			return Int{seq.len}
		}},
		NSFn{'=', fn (args List) !Type {
			b := args.nth(1) or { return error('=: missing param') }
			return make_bool(args.list[0].eq(b))
		}},
		NSFn{'<', fn (args List) !Type {
			b := args.nth(1) or { return error('<: missing param') }
			res := args.list[0].lt(b) or { return error('<: ${err}') }
			return make_bool(res)
		}},
		NSFn{'<=', fn (args List) !Type {
			b := args.nth(1) or { return error('<=: missing param') }
			res := args.list[0].lt(b) or { return error('<=: ${err}') }
			return make_bool(res || args.list[0].eq(b))
		}},
		NSFn{'>', fn (args List) !Type {
			b := args.nth(1) or { return error('>: missing param') }
			res := args.list[0].lt(b) or { return error('>: ${err}') }
			return make_bool(!res && !args.list[0].eq(b))
		}},
		NSFn{'>=', fn (args List) !Type {
			b := args.nth(1) or { return error('>=: missing param') }
			res := args.list[0].lt(b) or { return error('>=: ${err}') }
			return make_bool(!res)
		}},
		NSFn{'pr-str', fn (args List) !Type {
			return String{args.list.map(pr_str(it, true)).join(' ')}
		}},
		NSFn{'str', fn (args List) !Type {
			return String{args.list.map(pr_str(it, false)).join('')}
		}},
		NSFn{'prn', fn (args List) !Type {
			println(args.list.map(pr_str(it, true)).join(' '))
			return Nil{}
		}},
		NSFn{'println', fn (args List) !Type {
			println(args.list.map(pr_str(it, false)).join(' '))
			return Nil{}
		}},
	]
}
