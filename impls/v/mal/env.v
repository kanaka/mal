module mal

pub struct Env {
pub:
	outer &Env = unsafe { nil }
pub mut:
	data map[string]Type = map[string]Type{}
}

pub fn mk_env(outer &Env) Env {
	return Env{
		outer: unsafe { outer }
	}
}

pub fn (mut e Env) set(sym string, val Type) Type {
	e.data[sym] = val
	return val
}

pub fn (e Env) find(sym string) ?Type {
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

pub fn (e Env) get(sym string) !Type {
	return e.find(sym) or { error("'${sym}' not found") }
}
