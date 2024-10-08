call main
exit

#include "readline.rexx"
#include "reader.rexx"
#include "printer.rexx"
#include "types.rexx"
#include "env.rexx"
#include "core.rexx"

read: procedure expose values. err /* read(str) */
  return read_str(arg(1))

eval: procedure expose values. env. err /* eval(ast) */
  ast = arg(1)
  env_idx = arg(2)

  debug_eval = obj_type(env_get(env_idx, "DEBUG-EVAL"))
  if  debug_eval <> "ERR" & debug_eval <> "nill" & debug_eval <> "fals" then,
    call lineout , ("EVAL: " || print(ast))

  type = obj_type(ast)
  astval = obj_val(ast)
  select
    when type == "symb" then return env_get(env_idx, astval)
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
  a0sym = obj_val(a0)
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
    when a0sym == "do" then do
      res = "ERR"
      do i=2 to words(astval)
        res = eval(word(astval, i), env_idx)
        if res == "ERR" then return "ERR"
      end
      return res
    end
    when a0sym == "if" then do
      condval = eval(word(astval, 2), env_idx)
      if false?(condval) | nil?(condval) then
        if words(astval) >= 4 then
          return eval(word(astval, 4), env_idx)
        else
          return new_nil()
      else
        return eval(word(astval, 3), env_idx)
    end
    when a0sym == "fn*" then return new_func(word(astval, 3), env_idx, word(astval, 2))
    otherwise
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

      select
        when nativefn?(f) then do
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
          interpret "res = " || obj_val(f) || "(" || call_list || ")"
          return res
        end
        when func?(f) then do
          call_args = new_list(lst)
          return eval(func_body_ast(f), new_env(func_env_idx(f), func_binds(f), call_args))
        end
        otherwise
          err = "Unsupported function object type: " || obj_type(f)
          return "ERR"
        end
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

main:
  values. = ""
  values.0 = 0
  env. = ""
  env.0 = 0
  repl_env_idx = new_env(0)

  /* core.rexx: defined using Rexx */
  core_ns = get_core_ns()
  do i=1 to words(core_ns) by 2
    x = env_set(repl_env_idx, word(core_ns, i), new_nativefn(word(core_ns, i + 1)))
  end

  /* core.mal: defined using the language itself */
  x = re("(def! not (fn* (a) (if a false true)))")

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
