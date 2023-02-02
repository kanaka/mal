module mal

pub struct Env {
pub:
	outer &Env
pub mut:
	data map[string]MalType = map[string]MalType{}
}

pub fn mk_outer_env() Env {
	return Env{
		outer: unsafe { nil }
	}
}

pub fn mk_env(outer Env) Env {
	return Env{
		outer: &outer
	}
}

pub fn (mut e Env) set(sym string, val MalType) MalType {
	e.data[sym] = val
	return val
}

pub fn (e Env) find(sym string) ?MalType {
	$if env ? {
		println('ENV: looking for [${sym}] in\n${e.data}')
	}
	if res := e.data[sym] {
		$if env ? {
			println('...found')
		}
		return res
	}
	if e.outer != unsafe { nil } {
		$if env ? {
			println('...checking outer')
		}
		return e.outer.find(sym)
	}
	$if env ? {
		println('...not found')
	}
	return none
}

pub fn (e Env) get(sym string) !MalType {
	return e.find(sym) or { error('"${sym} not found') }
}
