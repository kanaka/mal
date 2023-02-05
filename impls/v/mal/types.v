module mal

type Type = False
	| Float
	| Fn
	| Hashmap
	| Int
	| Keyword
	| List
	| Nil
	| String
	| Symbol
	| True
	| Vector

fn implicit_conv(a Type, b Type) !(Type, Type) {
	// same type
	if a.type_idx() == b.type_idx() {
		return a, b
	}
	// automatic conversion
	if a is Int && b is Float {
		return Float{a.val}, b
	}
	if a is Float && b is Int {
		return a, Float{b.val}
	}
	if a is Vector && b is List {
		return List{a.vec}, b
	}
	if a is List && b is Vector {
		return a, List{b.vec}
	}
	// fail
	return error('type mismatch')
}

fn make_bool(cond bool) Type {
	return if cond { True{} } else { False{} }
}

pub fn (t Type) truthy() bool {
	return !t.falsey()
}

pub fn (t Type) falsey() bool {
	return t in [False, Nil]
}

pub fn (t Type) numeric() bool {
	return t in [Int, Float]
}

pub fn (t Type) sym() !string {
	return if t is Symbol { t.sym } else { error('symbol expected') }
}

type FnFn = fn (args List) !Type

pub fn (t Type) fn_() !FnFn {
	return if t is Fn { t.f } else { error('function expected') }
}

pub fn (t Type) int_() !i64 {
	return if t is Int { t.val } else { error('integer expected') }
}

pub fn (t Type) list() ![]Type {
	return if t is List { t.list } else { error('list expected') }
}

pub fn (t Type) list_or_vec() ![]Type {
	return match t {
		List { t.list }
		Vector { t.vec }
		Nil { []Type{} }
		// Nil { if allow_nil { []Type{} } else { error('list/vector expected') } }
		else { error('list/vector expected') }
	}
}

pub fn (t Type) eq(o Type) bool {
	a, b := implicit_conv(t, o) or { return false }
	match a {
		List {
			if a.list.len != (b as List).list.len {
				return false
			}
			for i, aa in a.list {
				if !aa.eq((b as List).list[i]) {
					return false
				}
			}
			return true
		}
		Vector {
			if a.vec.len != (b as Vector).vec.len {
				return false
			}
			for i, aa in a.vec {
				if !aa.eq((b as Vector).vec[i]) {
					return false
				}
			}
			return true
		}
		Int {
			return a.val == (b as Int).val
		}
		Float {
			return a.val == (b as Float).val
		}
		String {
			return a.val == (b as String).val
		}
		True {
			return b is True
		}
		False {
			return b is False
		}
		Nil {
			return b is Nil
		}
		Keyword {
			return a.kw == (b as Keyword).kw
		}
		Symbol {
			return a.sym == (b as Symbol).sym
		}
		Hashmap {
			return a.hm == (b as Hashmap).hm
		}
		Fn {
			return a.f == (b as Fn).f
		}
	}
}

pub fn (t Type) lt(o Type) !bool {
	a, b := implicit_conv(t, o)!
	return match a {
		Int { a.val < (b as Int).val }
		Float { a.val < (b as Float).val }
		String { a.val < (b as String).val }
		else { error('invalid comparison') }
	}
}

// --

pub struct Int {
pub:
	val i64
}

// --

pub struct Float {
pub:
	val f64
}

// --

pub struct String {
pub:
	val string
}

// -

pub struct Keyword {
pub:
	kw string
}

// --

pub struct Nil {}

// --

pub struct True {}

// --

pub struct False {}

// --

pub struct Symbol {
pub:
	sym string
}

// --

pub struct List {
pub:
	list []Type
}

pub fn (l List) first() !Type {
	return if l.list.len > 0 { l.list[0] } else { error('list: empty') }
}

pub fn (l List) last() !Type {
	return if l.list.len > 0 { l.list.last() } else { error('list: empty') }
}

pub fn (l List) rest() List {
	return if l.list.len > 1 { List{l.list[1..]} } else { List{} }
}

pub fn (l List) from(n int) List {
	return if l.list.len > n { List{l.list[n..]} } else { List{} }
}

pub fn (l List) len() int {
	return l.list.len
}

pub fn (l List) nth(n int) !Type {
	return if n < l.list.len { l.list[n] } else { error('list: index oob') }
}

// --

pub struct Vector {
pub:
	vec []Type
}

// --

pub struct Hashmap {
pub:
	hm map[string]Type
}

// --

pub struct Fn {
pub:
	// mut:
	//    env Env
	f FnFn
}
