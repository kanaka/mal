module mal

type MalType = MalFalse
	| MalFloat
	| MalFn
	| MalHashmap
	| MalInt
	| MalKeyword
	| MalList
	| MalNil
	| MalString
	| MalSymbol
	| MalTrue
	| MalVector

pub fn (t MalType) truthy() bool {
	return !t.falsey()
}

pub fn (t MalType) falsey() bool {
	return t is MalFalse || t is MalNil
}

pub fn (t MalType) sym() !string {
	return if t is MalSymbol { t.sym } else { error('symbol expected') }
}

type MalFnFn = fn (args MalList) !MalType

pub fn (t MalType) fn_() !MalFnFn {
	// BUG: https://github.com/vlang/v/issues/17204
	// return if t is MalFn { t.f } else { error( 'function expected' ) }
	if t is MalFn {
		return t.f
	} else {
		return error('function expected')
	}
}

pub fn (t MalType) int_() !i64 {
	return if t is MalInt { t.val } else { error('integer expected') }
}

pub fn (t MalType) list() ![]MalType {
	return if t is MalList { t.list } else { error('list expected') }
}

pub fn (t MalType) list_or_vec() ![]MalType {
	return match t {
		MalList { t.list }
		MalVector { t.vec }
		else { error('list/vector expected') }
	}
}

// --

pub struct MalInt {
pub:
	val i64
}

// --

pub struct MalFloat {
pub:
	val f64
}

// --

pub struct MalString {
pub:
	val string
}

// -

pub struct MalKeyword {
pub:
	key string
}

// --

pub struct MalNil {}

// --

pub struct MalTrue {}

// --

pub struct MalFalse {}

// --

pub struct MalSymbol {
pub:
	sym string
}

// --

pub struct MalList {
pub:
	list []MalType
}

pub fn (l MalList) first() !MalType {
	return if l.list.len > 0 { l.list[0] } else { error('list: empty') }
}

pub fn (l MalList) last() !MalType {
	return if l.list.len > 0 { l.list.last() } else { error('list: empty') }
}

pub fn (l MalList) rest() MalList {
	return if l.list.len > 0 { MalList{l.list[1..]} } else { MalList{} }
}

pub fn (l MalList) len() int {
	return l.list.len
}

pub fn (l MalList) nth(n int) !MalType {
	return if n < l.list.len { l.list[n] } else { error('list: index oob') }
}

// --

pub struct MalVector {
pub:
	vec []MalType
}

// --

pub struct MalHashmap {
pub:
	hm map[string]MalType
}

// --

pub struct MalFn {
pub:
	// mut:
	//    env Env
	f MalFnFn
}
