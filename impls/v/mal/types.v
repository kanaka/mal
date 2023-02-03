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

pub fn (t Type) truthy() bool {
	return !t.falsey()
}

pub fn (t Type) falsey() bool {
	return t is False || t is Nil
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
		else { error('list/vector expected') }
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
	key string
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
	return if l.list.len > 0 { List{l.list[1..]} } else { List{} }
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
