call main
exit

#include "readline.rexx"

read: procedure /* read(str) */
  return arg(1)

eval: procedure /* eval(exp, env) */
  return arg(1)

print: procedure /* print(exp) */
  return arg(1)

rep: procedure /* rep(str) */
  return print(eval(read(arg(1), "")))

main:
  do while lines() > 0 /* 1 == 1 */
    input_line = readline('user> ')
    if length(input_line) > 0 then
      call lineout , rep(input_line)
  end
