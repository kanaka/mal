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

pub struct MalInt {
pub:
	val i64
}

pub struct MalFloat {
pub:
	val f64
}

pub struct MalString {
pub:
	val string
}

pub struct MalKeyword {
pub:
	key string
}

pub struct MalNil {}

pub struct MalTrue {}

pub struct MalFalse {}

pub struct MalSymbol {
pub:
	sym string
}

pub struct MalList {
pub:
	list []MalType
}

pub struct MalVector {
pub:
	vec []MalType
}

pub struct MalHashmap {
pub:
	hm map[string]MalType
}

pub struct MalFn {
pub:
	f fn ([]MalType) !MalType
}
