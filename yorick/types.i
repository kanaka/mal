require, "hash.i"

struct MalError {
  string message
  pointer obj
}

struct MalNil {
  int val
}

MAL_NIL = MalNil()

struct MalTrue {
  int val
}

MAL_TRUE = MalTrue()

struct MalFalse {
  int val
}

MAL_FALSE = MalFalse()

struct MalNumber {
  int val
}

func new_number(s)
{
  return MalNumber(val=atoi(s))
}

struct MalSymbol {
  string val
  pointer meta
}

struct MalString {
  string val
  pointer meta
}

struct MalKeyword {
  string val
  pointer meta
}

struct MalList {
  pointer val
  pointer meta
}

struct MalVector {
  pointer val
  pointer meta
}

func count(obj) { return numberof(*obj.val); }

func rest(obj) {
  seq = count(obj) <= 1 ? [] : ((*obj.val)(2:))
  return MalList(val=&seq)
}

struct MalHashmap {
  pointer val
  pointer meta
}

func hashmap_obj_to_key(obj) {
  if (structof(obj) == MalString) return "str:" + obj.val
  else if (structof(obj) == MalSymbol) return "sym:" + obj.val
  else if (structof(obj) == MalKeyword) return "key:" + obj.val
  else error, "Unsupported obj type for hash key"
}

func hashmap_key_to_obj(key) {
  type_str = strpart(key, 1:4)
  val = strpart(key, 5:)
  if (type_str == "str:") return MalString(val=val)
  else if (type_str == "sym:") return MalSymbol(val=val)
  else if (type_str == "key:") return MalKeyword(val=val)
  else error, "Unsupported key type"
}

func array_to_hashmap(seq)
{
  if (numberof(seq) % 2 != 0) return MalError(message="Odd number of elements in hashmap")
  h = hash_new()
  for (i = 1; i <= numberof(seq); i += 2) {
    hash_set, h, hashmap_obj_to_key(*seq(i)), *seq(i + 1)
  }
  return MalHashmap(val=&h)
}

struct MalNativeFunction {
  string val
  pointer meta
}

struct MalFunction {
  pointer env
  pointer binds
  pointer ast
  int macro
  pointer meta
}

struct MalAtom {
  pointer val
  pointer meta
}

func is_macro(obj) { return (structof(obj) == MalFunction && obj.macro); }

struct MalAtomVal {
  pointer val
}

func new_boolean(b) {
  if (b) return MAL_TRUE
  return MAL_FALSE
}

func equal_seq(seq_a, seq_b) {
  if (numberof(seq_a) != numberof(seq_b)) return 0
  for (i = 1; i <= numberof(seq_a); ++i) {
    if (!equal(*seq_a(i), *seq_b(i))) return 0
  }
  return 1
}

func equal_hash(hm_a, hm_b) {
  if (numberof(*hm_a.keys) != numberof(*hm_b.keys)) return 0
  for (i = 1; i <= numberof(*hm_a.keys); ++i) {
    key_a = (*hm_a.keys)(i)
    val_a = *((*hm_a.vals)(i))
    val_b = hash_get(hm_b, key_a)
    if (is_void(val_b) || !equal(val_a, val_b)) return 0
  }
  return 1
}

func equal(a, b) {
  ta = structof(a)
  tb = structof(b)
  if (ta == MalNil) return tb == MalNil
  else if (ta == MalTrue) return tb == MalTrue
  else if (ta == MalFalse) return tb == MalFalse
  else if (ta == MalNumber) return tb == MalNumber && a.val == b.val
  else if (ta == MalSymbol) return tb == MalSymbol && a.val == b.val
  else if (ta == MalString) return tb == MalString && a.val == b.val
  else if (ta == MalKeyword) return tb == MalKeyword && a.val == b.val
  else if (ta == MalList || ta == MalVector) {
    return (tb == MalList || tb == MalVector) && equal_seq(*(a.val), *(b.val))
  }
  else if (ta == MalHashmap) return tb == MalHashmap && equal_hash(*a.val, *b.val)
  else return 0
}

func streplaceall(s, pattern, subst)
{
  return streplace(s, strfind(pattern, s, n=999), subst)
}
