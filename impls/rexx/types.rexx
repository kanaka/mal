#ifndef __types__
#define __types__

values. = ""
values.0 = 0

new_value_index: procedure expose values. /* new_value_index() */
  values.0 = values.0 + 1
  return values.0

obj_type: procedure /* obj_type(obj) */
  obj = arg(1)
  return left(obj, 4)

obj_val: procedure expose values. /* obj_val(obj) */
  obj = arg(1)
  type = obj_type(obj)
  val = substr(obj, 6)
  select
    when type == "numb" | type == "nill" | type == "true" | type == "fals" then return val
    otherwise
      return values.val
    end

obj_meta: procedure expose values. /* obj_meta(obj) */
  obj = arg(1)
  type = obj_type(obj)
  if type == "numb" | type == "nill" | type == "true" | type == "fals" then return ""
  ind = substr(obj, 6)
  return values.meta.ind

obj_clone_and_set_meta: procedure expose values. /* obj_clone_and_set_meta(obj, new_meta) */
  obj = arg(1)
  new_meta = arg(2)
  type = obj_type(obj)
  if type == "numb" | type == "nill" | type == "true" | type == "fals" then return ""
  orig_ind = substr(obj, 6)
  new_idx = new_value_index()
  values.new_idx = values.orig_ind
  values.meta.new_idx = new_meta
  return type || "_" || new_idx

new_number: procedure /* new_number(n) */
  n = arg(1)
  return "numb_" || n

number?: procedure /* number?(obj) */
  return obj_type(arg(1)) == "numb"

new_nil: procedure /* new_nil() */
  return "nill_0"

nil?: procedure /* nil?(obj) */
  return obj_type(arg(1)) == "nill"

new_true: procedure /* new_true() */
  return "true_0"

true?: procedure /* true?(obj) */
  return obj_type(arg(1)) == "true"

new_false: procedure /* new_false() */
  return "fals_0"

false?: procedure /* false?(obj) */
  return obj_type(arg(1)) == "fals"

new_boolean: procedure /* new_boolean(cond) */
  if arg(1) then
    return new_true()
  else
    return new_false()

new_symbol: procedure expose values. /* new_symbol(str) */
  str = arg(1)
  idx = new_value_index()
  values.idx = str
  return "symb_" || idx

symbol?: procedure /* symbol?(obj) */
  return obj_type(arg(1)) == "symb"

new_string: procedure expose values. /* new_string(str) */
  str = arg(1)
  idx = new_value_index()
  values.idx = str
  return "stri_" || idx

string?: procedure /* string?(obj) */
  return obj_type(arg(1)) == "stri"

new_keyword: procedure expose values. /* new_keyword(str) */
  str = arg(1)
  idx = new_value_index()
  values.idx = str
  return "keyw_" || idx

keyword?: procedure /* keyword?(obj) */
  return obj_type(arg(1)) == "keyw"

new_seq: procedure expose values. /* new_seq(type, seq) */
  type = arg(1)
  seq = arg(2)
  idx = new_value_index()
  values.idx = seq
  return type || "_" || idx

new_list: procedure expose values. /* new_list(seq) */
  seq = arg(1)
  return new_seq("list", seq)

list?: procedure /* list?(obj) */
  return obj_type(arg(1)) == "list"

new_vector: procedure expose values. /* new_vector(seq) */
  seq = arg(1)
  return new_seq("vect", seq)

vector?: procedure /* vector?(obj) */
  return obj_type(arg(1)) == "vect"

sequential?: procedure /* sequential?(obj) */
  return (list?(arg(1)) | vector?(arg(1)))

count_elements: procedure expose values. /* count_elements(lst) */
  return words(obj_val(arg(1)))

new_hashmap: procedure expose values. /* new_hashmap(seq) */
  seq = arg(1)
  return new_seq("hash", seq)

hashmap?: procedure /* hashmap?(obj) */
  return obj_type(arg(1)) == "hash"

contains?: procedure expose values. /* contains?(hm_val, key) */
  hm_val = arg(1)
  key = arg(2)
  do i=1 to words(hm_val) by 2
    if equal?(key, word(hm_val, i)) then return 1
  end
  return 0

hashmap_get: procedure expose values. /* hashmap_get(hm_val, key) */
  hm_val = arg(1)
  key = arg(2)
  do i=1 to words(hm_val) by 2
    if equal?(key, word(hm_val, i)) then return word(hm_val, i + 1)
  end
  return ""

new_nativefn: procedure expose values. /* new_hashmap(native_func_name) */
  native_func_name = arg(1)
  idx = new_value_index()
  values.idx = native_func_name
  return "nafn_" || idx

nativefn?: procedure /* nativefn?(obj) */
  return obj_type(arg(1)) == "nafn"

new_func: procedure expose values. /* new_func(body_ast, env_idx, binds) */
  body_ast = arg(1)
  env_idx = arg(2)
  binds = arg(3)
  is_macro = 0
  idx = new_value_index()
  values.idx = body_ast env_idx binds is_macro
  return "func_" || idx

func?: procedure /* func?(obj) */
  return obj_type(arg(1)) == "func"

func_macro?: procedure expose values. /* func_macro?(obj) */
  return func?(arg(1)) & (func_is_macro(arg(1)) == 1)

func_body_ast: procedure expose values. /* func_body_ast(func_obj) */
  return word(obj_val(arg(1)), 1)

func_env_idx: procedure expose values. /* func_env_idx(func_obj) */
  return word(obj_val(arg(1)), 2)

func_binds: procedure expose values. /* func_binds(func_obj) */
  return word(obj_val(arg(1)), 3)

func_is_macro: procedure expose values. /* func_is_macro(func_obj) */
  return word(obj_val(arg(1)), 4)

func_mark_as_macro: procedure expose values. /* func_mark_as_macro(func_obj) */
  idx = substr(arg(1), 6)
  values.idx = subword(values.idx, 1, 3) 1
  return arg(1)

new_atom: procedure expose values. /* new_atom(obj) */
  obj = arg(1)
  idx = new_value_index()
  values.idx = obj
  return "atom_" || idx

atom?: procedure /* atom?(obj) */
  return obj_type(arg(1)) == "atom"

atom_set: procedure expose values. /* atom_set(atom, new_value) */
  atom = arg(1)
  new_value = arg(2)
  idx = substr(atom, 6)
  values.idx = new_value
  return new_value

equal_hashmap?:  procedure expose values. /* equal_hashmap?(a, b) */
  hma_val = obj_val(arg(1))
  hmb_val = obj_val(arg(2))
  if words(hma_val) \= words(hmb_val) then return 0
  do i=1 to words(hma_val) by 2
    a_key = word(hma_val, i)
    a_val = word(hma_val, i + 1)
    b_val = hashmap_get(hmb_val, a_key)
    if b_val == "" then return 0
    if \equal?(a_val, b_val) then return 0
  end
  return 1

equal_sequential?: procedure expose values. /* equal_sequential?(a, b) */
  a_val = obj_val(arg(1))
  b_val = obj_val(arg(2))
  if words(a_val) \= words(b_val) then return 0
  do i=1 to words(a_val)
    if \equal?(word(a_val, i), word(b_val, i)) then return 0
  end
  return 1

equal?: procedure expose values. /* equal?(a, b) */
  a = arg(1)
  b = arg(2)
  a_type = obj_type(a)
  b_type = obj_type(b)
  a_val = obj_val(a)
  b_val = obj_val(b)
  select
    when nil?(a) then return nil?(b)
    when true?(a) then return true?(b)
    when false?(a) then return false?(b)
    when (a_type == "numb" & b_type = "numb") | ,
         (a_type == "symb" & b_type = "symb") | ,
         (a_type == "stri" & b_type = "stri") | ,
         (a_type == "keyw" & b_type = "keyw") then return (obj_val(a) == obj_val(b))
    when (sequential?(a) & sequential?(b)) then return equal_sequential?(a, b)
    when (hashmap?(a) & hashmap?(b)) then return equal_hashmap?(a, b)
    otherwise
      return 0
    end

#endif
