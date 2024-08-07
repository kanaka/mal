module mal

[heap]
pub struct Env {
pub:
	outer &Env = unsafe { nil }
pub mut:
	data map[string]Type = map[string]Type{}
}

pub fn mk_env(outer &Env) &Env {
	return &Env{
		outer: outer
	}
}

pub fn (mut e Env) bind(syms []string, args List) {
	for i, sym in syms {
		if sym == '&' {
			e.set(syms[i + 1], args.from(i))
			break
		} else {
			e.set(sym, args.nth(i))
		}
	}
}

pub fn (mut e Env) set(sym string, val Type) Type {
	$if env ? {
		println('SET(${e.level()}): ${sym} ${pr_str(val, true)}')
	}
	e.data[sym] = val
	return val
}

pub fn (e Env) find(sym string) ?Type {
	if res := e.data[sym] {
		$if env ? {
			println('GET(${e.level()}): ${sym} ...found')
		}
		return res
	}
	if e.outer != unsafe { nil } {
		$if env ? {
			println('GET(${e.level()}): ${sym} ...checking outer')
		}
		return e.outer.find(sym)
	}
	$if env ? {
		println('GET(${e.level()}): ${sym} ...not found')
	}
	return none
}

pub fn (e Env) get(sym string) !Type {
	return e.find(sym) or { error("'${sym}' not found") }
}

pub fn (e Env) level() int {
	return if e.outer == unsafe { nil } { 1 } else { 1 + e.outer.level() }
}
