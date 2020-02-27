require, "types.i"

func mal_equal(a) { return new_boolean(equal(*a(1), *a(2))); }
func mal_throw(a) { return MalError(obj=a(1)); }

func mal_nil_q(a) { return new_boolean(structof(*a(1)) == MalNil); }
func mal_true_q(a) { return new_boolean(structof(*a(1)) == MalTrue); }
func mal_false_q(a) { return new_boolean(structof(*a(1)) == MalFalse); }
func mal_string_q(a) { return new_boolean(structof(*a(1)) == MalString); }
func mal_symbol(a) { return MalSymbol(val=a(1)->val); }
func mal_symbol_q(a) { return new_boolean(structof(*a(1)) == MalSymbol); }
func mal_keyword(a) { return MalKeyword(val=a(1)->val); }
func mal_keyword_q(a) { return new_boolean(structof(*a(1)) == MalKeyword); }
func mal_number_q(a) { return new_boolean(structof(*a(1)) == MalNumber); }
func mal_fn_q(a)
{
  if (structof(*a(1)) == MalNativeFunction) return MAL_TRUE;
  return new_boolean(structof(*a(1)) == MalFunction && !a(1)->macro);
}
func mal_macro_q(a) { return new_boolean(structof(*a(1)) == MalFunction && a(1)->macro); }

func string_helper(a, delimiter, readable)
{
  res = ""
  for (i = 1; i <= numberof(a); ++i) {
    if (i > 1) res += delimiter
    res += pr_str(*a(i), readable)
  }
  return res
}

func mal_pr_str(a) { return MalString(val=string_helper(a, " ", 1)); }
func mal_str(a) { return MalString(val=string_helper(a, "", 0)); }
func mal_prn(a) { write, format="%s\n", string_helper(a, " ", 1); return MAL_NIL; }
func mal_println(a) { write, format="%s\n", string_helper(a, " ", 0); return MAL_NIL; }
func mal_read_string(a) { return read_str(a(1)->val); }

func mal_readline(a)
{
  extern stdin_file
  stdin_file = open("/dev/stdin", "r")
  write, format="%s", a(1)->val
  line = rdline(stdin_file, prompt="")
  return line ? MalString(val=line) : MAL_NIL
}

func mal_slurp(a)
{
  f = open(a(1)->val)
  lines = rdfile(f)
  close, f
  s = ""
  for (i = 1; i <= numberof(lines); ++i) {
    s += (lines(i) + "\n")
  }
  return MalString(val=s)
}

func mal_lt(a) { return new_boolean(a(1)->val < a(2)->val); }
func mal_lte(a) { return new_boolean(a(1)->val <= a(2)->val); }
func mal_gt(a) { return new_boolean(a(1)->val > a(2)->val); }
func mal_gte(a) { return new_boolean(a(1)->val >= a(2)->val); }

func mal_add(a) { return MalNumber(val=(a(1)->val + a(2)->val)); }
func mal_sub(a) { return MalNumber(val=(a(1)->val - a(2)->val)); }
func mal_mul(a) { return MalNumber(val=(a(1)->val * a(2)->val)); }
func mal_div(a) { return MalNumber(val=(a(1)->val / a(2)->val)); }

func mal_time_ms(a)
{
  elapsed = array(double, 3)
  timer, elapsed
  return MalNumber(val=floor(elapsed(3) * 1000))
}

func mal_list(a) { return MalList(val=&a); }
func mal_list_q(a) { return new_boolean(structof(*a(1)) == MalList); }
func mal_vector(a) { return MalVector(val=&a); }
func mal_vector_q(a) { return new_boolean(structof(*a(1)) == MalVector); }
func mal_hash_map(a) { return array_to_hashmap(a); }
func mal_map_q(a) { return new_boolean(structof(*a(1)) == MalHashmap); }

func mal_assoc(a) {
  h = *(a(1)->val)
  k1 = *h.keys
  v1 = *h.vals
  new_h = Hash(keys=&k1, vals=&v1)
  for (i = 2; i <= numberof(a); i += 2) {
    hash_set, new_h, hashmap_obj_to_key(*a(i)), *a(i + 1)
  }
  return MalHashmap(val=&new_h);
}

func mal_dissoc(a) {
  h = *(a(1)->val)
  k1 = *h.keys
  v1 = *h.vals
  new_h = Hash(keys=&k1, vals=&v1)
  for (i = 2; i <= numberof(a); ++i) {
    hash_delete, new_h, hashmap_obj_to_key(*a(i))
  }
  return MalHashmap(val=&new_h);
}

func mal_get(a) {
  if (structof(*a(1)) == MalNil) return MAL_NIL
  h = *(a(1)->val)
  key_obj = *a(2)
  val = hash_get(h, hashmap_obj_to_key(key_obj))
  return is_void(val) ? MAL_NIL : val
}

func mal_contains_q(a) {
  if (structof(*a(1)) == MalNil) return MAL_FALSE
  h = *(a(1)->val)
  key_obj = *a(2)
  return hash_has_key(h, hashmap_obj_to_key(key_obj)) ? MAL_TRUE : MAL_FALSE
}

func mal_keys(a) {
  keys_strs = *(a(1)->val->keys)
  if (numberof(keys_strs) == 0) return MalList(val=&[])
  res = array(pointer, numberof(keys_strs))
  for (i = 1; i <= numberof(keys_strs); ++i) {
    res(i) = &hashmap_key_to_obj(keys_strs(i))
  }
  return MalList(val=&res);
}

func mal_vals(a) { return MalList(val=a(1)->val->vals); }

func mal_sequential_q(a) { return new_boolean(structof(*a(1)) == MalList || structof(*a(1)) == MalVector); }

func mal_cons(a)
{
  a2_len = count(*a(2))
  seq = array(pointer, a2_len + 1)
  seq(1) = a(1)
  if (a2_len > 0) {
    seq(2:) = *(a(2)->val)
  }
  return MalList(val=&seq)
}

func mal_concat(a)
{
  seq = []
  for (i = 1; i <= numberof(a); ++i) {
    grow, seq, *(a(i)->val)
  }
  return MalList(val=&seq)
}

func mal_nth(a)
{
  index = a(2)->val
  if (index >= count(*a(1))) return MalError(message="nth: index out of range")
  return *((*(a(1)->val))(index + 1))
}

func mal_first(a)
{
  if (structof(*a(1)) == MalNil || count(*a(1)) == 0) return MAL_NIL
  return *((*(a(1)->val))(1))
}

func mal_rest(a)
{
  if (structof(*a(1)) == MalNil) return MalList(val=&[])
  return rest(*a(1))
}

func mal_empty_q(a) { return new_boolean((structof(*a(1)) == MalNil ? 1 : count(*a(1)) == 0)); }
func mal_count(a) { return MalNumber(val=(structof(*a(1)) == MalNil ? 0 : count(*a(1)))); }

func call_func(fn, args)
{
  if (structof(fn) == MalNativeFunction) {
    return call_core_fn(fn.val, args)
  } else if (structof(fn) == MalFunction) {
    fn_env = env_new(fn.env, binds=*fn.binds, exprs=args)
    return EVAL(*fn.ast, fn_env)
  } else {
    return MalError(message="Unknown function type")
  }
}

func mal_apply(a) {
  mid_args = numberof(a) > 2 ? a(2:-1) : []
  return call_func(*a(1), grow(mid_args, *(a(0)->val)))
}

func mal_map(a) {
  fn = *a(1)
  seq = *(a(2)->val)
  if (numberof(seq) == 0) return MalList(val=&[])
  new_seq = array(pointer, numberof(seq))
  for (i = 1; i <= numberof(seq); ++i) {
    new_val = call_func(fn, [seq(i)])
    if (structof(new_val) == MalError) return new_val
    new_seq(i) = &new_val
  }
  return MalList(val=&new_seq)
}

func mal_conj(a)
{
  obj = *a(1)
  type = structof(obj)
  if (type == MalList) {
    res = obj
    for (i = 2; i <= numberof(a); ++i) {
      res = mal_cons([a(i), &res])
    }
    return res
  } else if (type == MalVector) {
    seq = *obj.val
    grow, seq, a(2:)
    return MalVector(val=&seq)
  } else {
    return MalError(message="conj requires list or vector")
  }
}

func mal_seq(a)
{
  obj = *a(1)
  type = structof(obj)
  if (type == MalString) {
    len = strlen(obj.val)
    if (len == 0) return MAL_NIL
    seq = array(pointer, len)
    for (i = 1; i <= len; ++i) {
      seq(i) = &MalString(val=strpart(obj.val, i:i))
    }
    return MalList(val=&seq)
  } else if (type == MalList) {
    return count(obj) == 0 ? MAL_NIL : obj
  } else if (type == MalVector) {
    return count(obj) == 0 ? MAL_NIL : MalList(val=obj.val)
  } else if (type == MalNil) {
    return MAL_NIL
  } else {
    return MalError(message="seq requires string or list or vector or nil")
  }
}

func mal_meta(a)
{
  meta_obj = *(a(1)->meta)
  return is_void(meta_obj) ? MAL_NIL : meta_obj
}

func mal_with_meta(a)
{
  new_obj = *a(1)
  new_obj.meta = a(2)
  return new_obj
}

func mal_atom(a) { return MalAtom(val=&MalAtomVal(val=a(1))); }
func mal_atom_q(a) { return new_boolean(structof(*a(1)) == MalAtom); }
func mal_deref(a) { return *(a(1)->val->val); }
func mal_reset_bang(a) { a(1)->val->val = a(2); return *(a(1)->val->val); }
func mal_swap_bang(a)
{
  old_val = mal_deref([a(1)])
  args = array(pointer, numberof(a) - 1)
  args(1) = &old_val
  if (numberof(a) > 2) args(2:) = a(3:)
  new_val = call_func(*a(2), args)
  if (structof(new_val) == MalError) return new_val
  return mal_reset_bang([a(1), &new_val])
}

func mal_eval(a) { return EVAL(*a(1), repl_env); }

func yorick_to_mal(e)
{
  if (is_void(e)) return MAL_NIL
  if (is_scalar(e)) {
    if (is_numerical(e)) return MalNumber(val=e)
    else if (is_string(e)) return MalString(val=e)
    else return MalString(val=totxt(e))
  } else {
    seq = array(pointer, numberof(e))
    for (i = 1; i <= numberof(e); ++i) {
      seq(i) = &yorick_to_mal(e(i))
    }
    return MalList(val=&seq)
  }
}

func mal_yorick_eval(a) { return yorick_to_mal(exec(a(1)->val)); }

core_ns = h_new()

h_set, core_ns, "=",           mal_equal
h_set, core_ns, "throw",       mal_throw

h_set, core_ns, "nil?",        mal_nil_q
h_set, core_ns, "true?",       mal_true_q
h_set, core_ns, "false?",      mal_false_q
h_set, core_ns, "string?",     mal_string_q
h_set, core_ns, "symbol",      mal_symbol
h_set, core_ns, "symbol?",     mal_symbol_q
h_set, core_ns, "keyword",     mal_keyword
h_set, core_ns, "keyword?",    mal_keyword_q
h_set, core_ns, "number?",     mal_number_q
h_set, core_ns, "fn?",         mal_fn_q
h_set, core_ns, "macro?",      mal_macro_q

h_set, core_ns, "pr-str",      mal_pr_str
h_set, core_ns, "str",         mal_str
h_set, core_ns, "prn",         mal_prn
h_set, core_ns, "println",     mal_println
h_set, core_ns, "read-string", mal_read_string
h_set, core_ns, "readline",    mal_readline
h_set, core_ns, "slurp",       mal_slurp

h_set, core_ns, "<",           mal_lt
h_set, core_ns, "<=",          mal_lte
h_set, core_ns, ">",           mal_gt
h_set, core_ns, ">=",          mal_gte
h_set, core_ns, "+",           mal_add
h_set, core_ns, "-",           mal_sub
h_set, core_ns, "*",           mal_mul
h_set, core_ns, "/",           mal_div
h_set, core_ns, "time-ms",     mal_time_ms

h_set, core_ns, "list",        mal_list
h_set, core_ns, "list?",       mal_list_q
h_set, core_ns, "vector",      mal_vector
h_set, core_ns, "vector?",     mal_vector_q
h_set, core_ns, "hash-map",    mal_hash_map
h_set, core_ns, "map?",        mal_map_q
h_set, core_ns, "assoc",       mal_assoc
h_set, core_ns, "dissoc",      mal_dissoc
h_set, core_ns, "get",         mal_get
h_set, core_ns, "contains?",   mal_contains_q
h_set, core_ns, "keys",        mal_keys
h_set, core_ns, "vals",        mal_vals

h_set, core_ns, "sequential?", mal_sequential_q
h_set, core_ns, "cons",        mal_cons
h_set, core_ns, "concat",      mal_concat
h_set, core_ns, "nth",         mal_nth
h_set, core_ns, "first",       mal_first
h_set, core_ns, "rest",        mal_rest
h_set, core_ns, "empty?",      mal_empty_q
h_set, core_ns, "count",       mal_count
h_set, core_ns, "apply",       mal_apply
h_set, core_ns, "map",         mal_map

h_set, core_ns, "conj",        mal_conj
h_set, core_ns, "seq",         mal_seq

h_set, core_ns, "meta",        mal_meta
h_set, core_ns, "with-meta",   mal_with_meta
h_set, core_ns, "atom",        mal_atom
h_set, core_ns, "atom?",       mal_atom_q
h_set, core_ns, "deref",       mal_deref
h_set, core_ns, "reset!",      mal_reset_bang
h_set, core_ns, "swap!",       mal_swap_bang

h_set, core_ns, "eval",        mal_eval
h_set, core_ns, "yorick-eval", mal_yorick_eval

func call_core_fn(name, args_list)
{
    f = h_get(core_ns, name)
    return f(args_list)
}
