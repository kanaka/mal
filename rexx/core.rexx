#ifndef __core__
#define __core__

#include "types.rexx"

mal_equal?: procedure expose values. /* mal_equal?(a, b) */
  return new_boolean(equal?(arg(1), arg(2)))

mal_throw: procedure expose values. err /* mal_throw(a) */
  err = "__MAL_EXCEPTION__" arg(1)
  return "ERR"

mal_nil?: procedure expose values. /* mal_nil?(a) */
  return new_boolean(nil?(arg(1)))

mal_true?: procedure expose values. /* mal_true?(a) */
  return new_boolean(true?(arg(1)))

mal_false?: procedure expose values. /* mal_false?(a) */
  return new_boolean(false?(arg(1)))

mal_string?: procedure expose values. /* mal_string?(a) */
  return new_boolean(string?(arg(1)))

mal_symbol: procedure expose values. /* mal_symbol(a) */
  return new_symbol(obj_val(arg(1)))

mal_symbol?: procedure expose values. /* mal_symbol?(a) */
  return new_boolean(symbol?(arg(1)))

mal_keyword: procedure expose values. /* mal_keyword(a) */
  return new_keyword(obj_val(arg(1)))

mal_keyword?: procedure expose values. /* mal_keyword?(a) */
  return new_boolean(keyword?(arg(1)))

mal_number?: procedure expose values. /* mal_number?(a) */
  return new_boolean(number?(arg(1)))

mal_fn?: procedure expose values. /* mal_fn?(a) */
  return new_boolean(nativefn?(arg(1)) | (func?(arg(1)) & (func_is_macro(arg(1)) \= 1)))

mal_macro?: procedure expose values. /* mal_macro?(a) */
  return new_boolean(func_macro?(arg(1)))

mal_pr_str: procedure expose values. /* mal_pr_str(...) */
  res = ""
  do i=1 to arg()
    element = pr_str(arg(i), 1)
    if i == 1 then
      res = element
    else
      res = res || " " || element
  end
  return new_string(res)

mal_str: procedure expose values. /* mal_str(...) */
  res = ""
  do i=1 to arg()
    element = pr_str(arg(i), 0)
    if i == 1 then
      res = element
    else
      res = res || element
  end
  return new_string(res)

mal_prn: procedure expose values. /* mal_prn(...) */
  res = ""
  do i=1 to arg()
    element = pr_str(arg(i), 1)
    if i == 1 then
      res = element
    else
      res = res || " " || element
  end
  say res
  return new_nil()

mal_println: procedure expose values. /* mal_println(...) */
  res = ""
  do i=1 to arg()
    element = pr_str(arg(i), 0)
    if i == 1 then
      res = element
    else
      res = res || " " || element
  end
  say res
  return new_nil()

mal_read_string: procedure expose values. err /* mal_read_string(str) */
  return read_str(obj_val(arg(1)))

mal_readline: procedure expose values. /* mal_readline(prompt) */
  line = readline(obj_val(arg(1)))
  if length(line) > 0 then return new_string(line)
  if lines() > 0 then return new_string("")
  return new_nil()

mal_slurp: procedure expose values. /* mal_read_string(filename) */
  file_content = charin(obj_val(arg(1)), 1, 100000)
  return new_string(file_content)

mal_lt: procedure expose values. /* mal_lt(a, b) */
  return new_boolean(obj_val(arg(1)) < obj_val(arg(2)))

mal_lte: procedure expose values. /* mal_lte(a, b) */
  return new_boolean(obj_val(arg(1)) <= obj_val(arg(2)))

mal_gt: procedure expose values. /* mal_gt(a, b) */
  return new_boolean(obj_val(arg(1)) > obj_val(arg(2)))

mal_gte: procedure expose values. /* mal_gte(a, b) */
  return new_boolean(obj_val(arg(1)) >= obj_val(arg(2)))

mal_add: procedure expose values. /* mal_add(a, b) */
  return new_number(obj_val(arg(1)) + obj_val(arg(2)))

mal_sub: procedure expose values. /* mal_sub(a, b) */
  return new_number(obj_val(arg(1)) - obj_val(arg(2)))

mal_mul: procedure expose values. /* mal_mul(a, b) */
  return new_number(obj_val(arg(1)) * obj_val(arg(2)))

mal_div: procedure expose values. /* mal_div(a, b) */
  return new_number(obj_val(arg(1)) / obj_val(arg(2)))

mal_time_ms: procedure expose values. /* mal_time_ms() */
  return new_number(trunc(time('E') * 1000))

mal_list: procedure expose values. /* mal_list(...) */
  res = ""
  do i=1 to arg()
    if i == 1 then
      res = arg(i)
    else
      res = res || " " || arg(i)
  end
  return new_list(res)

mal_list?: procedure expose values. /* mal_list?(a) */
  return new_boolean(list?(arg(1)))

mal_vector: procedure expose values. /* mal_vector(...) */
  res = ""
  do i=1 to arg()
    if i == 1 then
      res = arg(i)
    else
      res = res || " " || arg(i)
  end
  return new_vector(res)

mal_vector?: procedure expose values. /* mal_vector?(a) */
  return new_boolean(vector?(arg(1)))

mal_hash_map: procedure expose values. /* mal_hash_map(...) */
  res = ""
  do i=1 to arg()
    if i == 1 then
      res = arg(i)
    else
      res = res || " " || arg(i)
  end
  return new_hashmap(res)

mal_map?: procedure expose values. /* mal_map?(a) */
  return new_boolean(hashmap?(arg(1)))

mal_assoc: procedure expose values. /* mal_assoc(a, ...) */
  hm = arg(1)
  res = ""
  do i=2 to arg() by 2
    key_val = arg(i) || " " || arg(i + 1)
    if res == 2 then
      res = key_val
    else
      res = res || " " || key_val
  end
  hm_val = obj_val(hm)
  do i=1 to words(hm_val) by 2
    if \contains?(res, word(hm_val, i)) then
      res = res || " " || word(hm_val, i) || " " || word(hm_val, i + 1)
  end
  return new_hashmap(res)

mal_dissoc: procedure expose values. /* mal_dissoc(a, ...) */
  hm = arg(1)
  res = ""
  hm_val = obj_val(hm)
  do i=1 to words(hm_val) by 2
    key = word(hm_val, i)
    found = 0
    do j=2 to arg()
      if equal?(key, arg(j)) then do
        found = 1
        leave
      end
    end
    if \found then do
      if length(res) > 0 then res = res || " "
      res = res || key || " " || word(hm_val, i + 1)
    end
  end
  return new_hashmap(res)

mal_get: procedure expose values. /* mal_get(a, b) */
  res = hashmap_get(obj_val(arg(1)), arg(2))
  if res == "" then
    return new_nil()
  else
    return res

mal_contains?: procedure expose values. /* mal_contains?(a, b) */
  return new_boolean(contains?(obj_val(arg(1)), arg(2)))

mal_keys: procedure expose values. /* mal_keys(a) */
  hm_val = obj_val(arg(1))
  seq = ""
  do i=1 to words(hm_val) by 2
    if i == 1 then
      seq = word(hm_val, i)
    else
      seq = seq || " " || word(hm_val, i)
  end
  return new_list(seq)

mal_vals: procedure expose values. /* mal_vals(a) */
  hm_val = obj_val(arg(1))
  seq = ""
  do i=2 to words(hm_val) by 2
    if i == 1 then
      seq = word(hm_val, i)
    else
      seq = seq || " " || word(hm_val, i)
  end
  return new_list(seq)

mal_sequential?: procedure expose values. /* mal_sequential?(a) */
  return new_boolean(sequential?(arg(1)))

mal_cons: procedure expose values. /* mal_cons(a, b) */
  return new_list(arg(1) || " " || obj_val(arg(2)))

mal_concat: procedure expose values. /* mal_concat(...) */
  seq = ""
  do i=1 to arg()
    if i == 1 then
      seq = obj_val(arg(i))
    else
      seq = seq || " " || obj_val(arg(i))
  end
  return new_list(seq)

mal_nth: procedure expose values. err /* mal_nth(list, index) */
  list_val = obj_val(arg(1))
  i = obj_val(arg(2))
  if i >= words(list_val) then do
    err = "nth: index out of range"
    return "ERR"
  end
  return word(list_val, i + 1)

mal_first: procedure expose values. /* mal_first(a) */
  if nil?(arg(1)) then return new_nil()
  list_val = obj_val(arg(1))
  if words(list_val) == 0 then return new_nil()
  return word(list_val, 1)

mal_rest: procedure expose values. /* mal_rest(a) */
  return new_list(subword(obj_val(arg(1)), 2))

mal_empty?: procedure expose values. /* mal_empty?(a) */
  if nil?(arg(1)) then return new_true()
  return new_boolean(count_elements(arg(1)) == 0)

mal_count: procedure expose values. /* mal_count(a) */
  if nil?(arg(1)) then return new_number(0)
  return new_number(count_elements(arg(1)))

apply_function: procedure expose values. env. err /* apply_function(fn, lst) */
  f = arg(1)
  call_args = arg(2)
  select
    when nativefn?(f) then do
      call_args_val = obj_val(call_args)
      call_list = ""
      do i=1 to words(call_args_val)
        element = '"' || word(call_args_val, i) || '"'
        if i > 1 then
          call_list = call_list || ', ' || element
        else
          call_list = element
      end
      res = ""
      interpret "res = " || obj_val(f) || "(" || call_list || ")"
      return res
    end
    when func?(f) then do
      apply_env_idx = new_env(func_env_idx(f), func_binds(f), call_args)
      return eval(func_body_ast(f), apply_env_idx)
    end
    otherwise
      err = "Unsupported function object type: " || obj_type(f)
      return "ERR"
    end

mal_apply: procedure expose values. env. err /* mal_apply(fn, ..., lst) */
  fn = arg(1)
  seq = ""
  do i=2 to (arg() - 1)
    if i == 2 then
      seq = arg(i)
    else
      seq = seq || " " || arg(i)
  end
  if arg() > 1 then do
    seq = seq || " " || obj_val(arg(arg()))
  end
  return apply_function(fn, new_list(seq))

mal_map: procedure expose values. env. err /* mal_map(f, lst) */
  fn = arg(1)
  lst_val = obj_val(arg(2))
  res = ""
  do i=1 to words(lst_val)
    element = word(lst_val, i)
    mapped_element = apply_function(fn, new_list(element))
    if mapped_element == "ERR" then return "ERR"
    if i == 1 then
      res = mapped_element
    else
      res = res || " " || mapped_element
  end
  return new_list(res)

mal_conj: procedure expose values. env. err /* mal_conj(a, ...) */
  a = arg(1)
  select
    when list?(a) then do
      do i=2 to arg()
        a = mal_cons(arg(i), a)
      end
      return a
    end
    when vector?(a) then do
      seq = obj_val(a)
      do i=2 to arg()
        if length(seq) > 0 then seq = seq || " "
        seq = seq || arg(i)
      end
      return new_vector(seq)
    end
    otherwise
      err = "conj requires list or vector"
      return "ERR"
    end

mal_seq: procedure expose values. env. err /* mal_conj(a) */
  a = arg(1)
  select
    when string?(a) then do
      str = obj_val(a)
      if length(str) == 0 then return new_nil()
      seq = ""
      do i=1 to length(str)
        element = new_string(substr(str, i, 1))
        if i == 1 then
          seq = element
        else
          seq = seq || " " || element
      end
      return new_list(seq)
    end
    when list?(a) then do
      if count_elements(a) == 0 then return new_nil()
      return a
    end
    when vector?(a) then do
      if count_elements(a) == 0 then return new_nil()
      return new_list(obj_val(a))
    end
    when nil?(a) then return new_nil()
    otherwise
      err = "seq requires string or list or vector or nil"
      return "ERR"
    end

mal_with_meta: procedure expose values. /* mal_with_meta(a, b) */
  new_obj = obj_clone_and_set_meta(arg(1), arg(2))
  if new_obj == "" then return arg(1)
  return new_obj

mal_meta: procedure expose values. /* mal_meta(a) */
  meta = obj_meta(arg(1))
  if meta == "" then return new_nil()
  return meta

mal_atom: procedure expose values. /* mal_atom(a) */
  return new_atom(arg(1))

mal_atom?: procedure expose values. /* mal_atom?(a) */
  return new_boolean(atom?(arg(1)))

mal_deref: procedure expose values. /* mal_deref(a) */
  return obj_val(arg(1))

mal_reset!: procedure expose values. /* mal_reset!(a, new_val) */
  return atom_set(arg(1), arg(2))

mal_swap!: procedure expose values. env. err /* mal_swap!(a, fn, ...) */
  atom = arg(1)
  fn = arg(2)
  atom_val = obj_val(atom)
  seq = atom_val
  do i=3 to arg()
    seq = seq || " " || arg(i)
  end
  new_val = apply_function(fn, new_list(seq))
  if new_val == "ERR" then return "ERR"
  return atom_set(atom, new_val)

mal_rexx_eval: procedure expose values. /* mal_rexx_eval(..., a) */
  do i=1 to (arg() - 1)
    interpret obj_val(arg(i))
  end
  last_arg = arg(arg())
  if nil?(last_arg) then return new_nil()
  last_arg_str = obj_val(last_arg)
  if length(last_arg_str) == 0 then return new_nil()
  rexx_eval_res = ""
  interpret "rexx_eval_res = " || last_arg_str
  if datatype(rexx_eval_res) == "NUM" then
    return new_number(rexx_eval_res)
  else
    return new_string(rexx_eval_res)

get_core_ns: procedure /* get_core_ns() */
  return "=           mal_equal?"      ,
         "throw       mal_throw"       ,
                                       ,
         "nil?        mal_nil?"        ,
         "true?       mal_true?"       ,
         "false?      mal_false?"      ,
         "string?     mal_string?"     ,
         "symbol      mal_symbol"      ,
         "symbol?     mal_symbol?"     ,
         "keyword     mal_keyword"     ,
         "keyword?    mal_keyword?"    ,
         "number?     mal_number?"     ,
         "fn?         mal_fn?"         ,
         "macro?      mal_macro?"      ,
                                       ,
         "pr-str      mal_pr_str"      ,
         "str         mal_str"         ,
         "prn         mal_prn"         ,
         "println     mal_println"     ,
         "read-string mal_read_string" ,
         "readline    mal_readline"    ,
         "slurp       mal_slurp"       ,
                                       ,
         "<           mal_lt"          ,
         "<=          mal_lte"         ,
         ">           mal_gt"          ,
         ">=          mal_gte"         ,
         "+           mal_add"         ,
         "-           mal_sub"         ,
         "*           mal_mul"         ,
         "/           mal_div"         ,
         "time-ms     mal_time_ms"     ,
                                       ,
         "list        mal_list"        ,
         "list?       mal_list?"       ,
         "vector      mal_vector"      ,
         "vector?     mal_vector?"     ,
         "hash-map    mal_hash_map"    ,
         "map?        mal_map?"        ,
         "assoc       mal_assoc"       ,
         "dissoc      mal_dissoc"      ,
         "get         mal_get"         ,
         "contains?   mal_contains?"   ,
         "keys        mal_keys"        ,
         "vals        mal_vals"        ,
                                       ,
         "sequential? mal_sequential?" ,
         "cons        mal_cons"        ,
         "concat      mal_concat"      ,
         "nth         mal_nth"         ,
         "first       mal_first"       ,
         "rest        mal_rest"        ,
         "empty?      mal_empty?"      ,
         "count       mal_count"       ,
         "apply       mal_apply"       ,
         "map         mal_map"         ,
                                       ,
         "conj        mal_conj"        ,
         "seq         mal_seq"         ,
                                       ,
         "meta        mal_meta"        ,
         "with-meta   mal_with_meta"   ,
         "atom        mal_atom"        ,
         "atom?       mal_atom?"       ,
         "deref       mal_deref"       ,
         "reset!      mal_reset!"      ,
         "swap!       mal_swap!"       ,
                                       ,
         "rexx-eval   mal_rexx_eval"

#endif
