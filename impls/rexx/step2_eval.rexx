call main
exit

#include "readline.rexx"
#include "reader.rexx"
#include "printer.rexx"
#include "types.rexx"

read: procedure expose values. err /* read(str) */
  return read_str(arg(1))

eval: procedure expose values. env. err /* eval(ast) */
  ast = arg(1)

  --  call lineout , ("EVAL: " || print(ast))

  type = obj_type(ast)
  astval = obj_val(ast)
  select
    when type == "symb" then do
      varname = astval
      if env.varname == "" then do
        err = "'" || varname || "' not found"
        return "ERR"
      end
      return env.varname
    end
    when type == "list" & words(astval) > 0 then do
      --  proceed after this select statement
    end
    when type == "vect" then do
      res = ""
      do i=1 to words(astval)
        element = eval(word(astval, i), env_idx)
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
      do i=1 to words(astval)
        element = eval(word(astval, i), env_idx)
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

  --  ast is a non-empty list

  a0 = word(astval, 1)
  f = eval(a0, env_idx)
  if f == "ERR" then return "ERR"

  --  Evaluate the arguments and store them to lst.
  lst = ""
  do i=2 to words(astval)
    element = eval(word(astval, i), env_idx)
    if element == "ERR" then return "ERR"
    if i > 2 then
      lst = lst || " " || element
    else
      lst = element
  end

  call_args = lst
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

print: procedure expose values. /* print(ast) */
  return pr_str(arg(1), 1)

rep: procedure expose values. env. err /* rep(str) */
  ast = read(arg(1))
  if ast == "ERR" then return "ERR"
  exp = eval(ast)
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
  key = "+" ; env.key = "mal_add"
  key = "-" ; env.key = "mal_sub"
  key = "*" ; env.key = "mal_mul"
  key = "/" ; env.key = "mal_div"
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
