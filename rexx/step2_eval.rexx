call main
exit

#include "readline.rexx"
#include "reader.rexx"
#include "printer.rexx"
#include "types.rexx"

read: procedure expose values. err /* read(str) */
  return read_str(arg(1))

eval_ast: procedure expose values. env. err /* eval_ast(ast) */
  ast = arg(1)
  type = obj_type(ast)
  val = obj_val(ast)
  select
    when type == "symb" then do
      varname = val
      if env.varname == "" then do
        err = "'" || varname || "' not found"
        return "ERR"
      end
      return env.varname
    end
    when type == "list" then do
      res = ""
      do i=1 to words(val)
        element = eval(word(val, i))
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
        element = eval(word(val, i))
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
        element = eval(word(val, i))
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
  if \list?(ast) then return eval_ast(ast)
  astval = obj_val(ast)
  if words(astval) == 0 then return ast
  lst_obj = eval_ast(ast)
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
