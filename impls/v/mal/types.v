module mal

type MalType = MalFalse
	| MalFloat
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
	hash map[string]MalType
}

pub fn mk_hashmap(list []MalType) !MalHashmap {
	mut list_ := list[0..]
	mut hash := map[string]MalType{}
	for {
		if list_.len == 0 {
			break
		}
		if list_.len == 1 {
			return error('odd number of hashmap args')
		}
		key, val := list_[0], list_[1]
		if key is MalString {
			hash['"${key.val}"'] = val
		} else if key is MalKeyword {
			hash[':${key.key}'] = val
		} else {
			return error('bad key type in hashmap')
		}
		list_ = list_[2..]
	}
	return MalHashmap{hash}
}
