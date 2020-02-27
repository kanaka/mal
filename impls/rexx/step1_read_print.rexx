call main
exit

#include "readline.rexx"
#include "reader.rexx"
#include "printer.rexx"

read: procedure expose values. err /* read(str) */
  return read_str(arg(1))

eval: procedure expose values. /* eval(exp, env) */
  return arg(1)

print: procedure expose values. /* print(exp) */
  return pr_str(arg(1), 1)

rep: procedure expose values. env. err /* rep(str) */
  ast = read(arg(1))
  if ast == "ERR" then return "ERR"
  exp = eval(ast)
  return print(exp)

main:
  values. = ""
  values.0 = 0
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
