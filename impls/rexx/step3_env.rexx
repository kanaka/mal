call main
exit

#include "readline.rexx"
#include "reader.rexx"
#include "printer.rexx"
#include "types.rexx"
#include "env.rexx"

read: procedure expose values. err /* read(str) */
  return read_str(arg(1))

eval_ast: procedure expose values. env. err /* eval_ast(ast, env_idx) */
  ast = arg(1)
  env_idx = arg(2)
  type = obj_type(ast)
  val = obj_val(ast)
  select
    when type == "symb" then return env_get(env_idx, val)
    when type == "list" then do
      res = ""
      do i=1 to words(val)
        element = eval(word(val, i), env_idx)
        if element == "ERR" then return "ERR"
        if i > 1 then
          res = res || " " || element
        else
          res = element
      end
      return new_list(res)
    end
    when type == "vect" then do
      res = ""
      do i=1 to words(val)
        element = eval(word(val, i), env_idx)
        if element == "ERR" then return "ERR"
        if i > 1 then
          res = res || " " || element
        else
          res = element
      end
      return new_vector(res)
    end
    when type == "hash" then do
      res = ""
      do i=1 to words(val)
        element = eval(word(val, i), env_idx)
        if element == "ERR" then return "ERR"
        if i > 1 then
          res = res || " " || element
        else
          res = element
      end
      return new_hashmap(res)
    end
    otherwise
      return ast
    end

eval: procedure expose values. env. err /* eval(ast) */
  ast = arg(1)
  env_idx = arg(2)
  if \list?(ast) then return eval_ast(ast, env_idx)
  astval = obj_val(ast)
  if words(astval) == 0 then return ast
  a0sym = obj_val(word(astval, 1))
  select
    when a0sym == "def!" then do
      a1sym = obj_val(word(astval, 2))
      a2 = eval(word(astval, 3), env_idx)
      if a2 == "ERR" then return "ERR"
      return env_set(env_idx, a1sym, a2)
    end
    when a0sym == "let*" then do
      a1lst = obj_val(word(astval, 2))
      letenv_idx = new_env(env_idx)
      do i=1 to words(a1lst) by 2
        k = obj_val(word(a1lst, i))
        v = eval(word(a1lst, i + 1), letenv_idx)
        if v == "ERR" then return "ERR"
        unused = env_set(letenv_idx, k, v)
      end
      return eval(word(astval, 3), letenv_idx)
    end
    otherwise
      lst_obj = eval_ast(ast, env_idx)
      if lst_obj == "ERR" then return "ERR"
      lst = obj_val(lst_obj)
      f = word(lst, 1)
      call_args = subword(lst, 2)
      call_list = ""
      do i=1 to words(call_args)
        element = '"' || word(call_args, i) || '"'
        if i > 1 then
          call_list = call_list || ', ' || element
        else
          call_list = element
      end
      res = ""
      interpret "res = " || f || "(" || call_list || ")"
      return res
    end

print: procedure expose values. /* print(ast) */
  return pr_str(arg(1), 1)

re:  procedure expose values. env. err repl_env_idx /* re(str) */
  str = arg(1)
  ast = read(str)
  if ast == "ERR" then return "ERR"
  return eval(ast, repl_env_idx)

rep: procedure expose values. env. err repl_env_idx /* rep(str) */
  str = arg(1)
  exp = re(str)
  if exp == "ERR" then return "ERR"
  return print(exp)

mal_add: procedure expose values. /* mal_add(a, b) */
  return new_number(obj_val(arg(1)) + obj_val(arg(2)))

mal_sub: procedure expose values. /* mal_sub(a, b) */
  return new_number(obj_val(arg(1)) - obj_val(arg(2)))

mal_mul: procedure expose values. /* mal_mul(a, b) */
  return new_number(obj_val(arg(1)) * obj_val(arg(2)))

mal_div: procedure expose values. /* mal_div(a, b) */
  return new_number(obj_val(arg(1)) / obj_val(arg(2)))

main:
  values. = ""
  values.0 = 0
  env. = ""
  env.0 = 0
  repl_env_idx = new_env(0)
  x = env_set(repl_env_idx, "+", "mal_add")
  x = env_set(repl_env_idx, "-", "mal_sub")
  x = env_set(repl_env_idx, "*", "mal_mul")
  x = env_set(repl_env_idx, "/", "mal_div")
  err = ""
  do while lines() > 0 /* 1 == 1 */
    input_line = readline('user> ')
    if length(input_line) > 0 then do
      res = rep(input_line)
      if res == "ERR" then
        call lineout , "Error: " || err
      else
        call lineout , res
    end
  end
