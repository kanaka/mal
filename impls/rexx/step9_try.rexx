/* Save command-line arguments from the top-level program before entering a procedure */
command_line_args. = ""
command_line_args.0 = arg()
do i=1 to command_line_args.0
  command_line_args.i = arg(i)
end

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

starts_with?: procedure expose values. /* starts_with?(lst, sym) */
  lst = arg(1)
  sym = arg(2)
  if words(obj_val(lst)) <> 2 then return 0
  a0 = word(obj_val(lst), 1)
  return symbol?(a0) & obj_val(a0) == sym

qq_loop: procedure expose values. /* qq_loop(elt, acc) */
  elt = arg(1)
  acc = arg(2)
  if list?(elt) & starts_with?(elt, "splice-unquote") then
    return new_list(new_symbol("concat") || " " || word(obj_val(elt), 2) || " " || acc)
  else
    return new_list(new_symbol("cons") || " " || quasiquote(elt) || " " || acc)

qq_foldr: procedure expose values. /* qq_foldr(xs) */
  xs = arg(1)
  acc = new_list()
  do i=words(xs) to 1 by -1
    acc = qq_loop(word(xs, i), acc)
  end
  return acc

quasiquote: procedure expose values. env. err /* quasiquote(ast) */
  ast = arg(1)
  type = obj_type(ast)
  select
    when type == "list" then
        if starts_with?(ast, "unquote") then
          return word(obj_val(ast), 2)
        else
          return qq_foldr(obj_val(ast))
    when type == "vect" then
      return new_list(new_symbol("vec") || " " || qq_foldr(obj_val(ast)))
    when type == "symb" | type == "hash" then
      return new_list(new_symbol("quote") || " " || ast)
    otherwise
      return ast
    end

eval: procedure expose values. env. err /* eval(ast) */
  ast = arg(1)
  env_idx = arg(2)
  do forever

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
        env_idx = letenv_idx
        ast = word(astval, 3)
        /* TCO */
      end
      when a0sym == "quote" then return word(astval, 2)
      when a0sym == "quasiquote" then do
        ast = quasiquote(word(astval, 2))
        /* TCO */
      end
      when a0sym == "defmacro!" then do
        a1sym = obj_val(word(astval, 2))
        a2 = eval(word(astval, 3), env_idx)
        if a2 == "ERR" then return "ERR"
        return env_set(env_idx, a1sym, func_mark_as_macro(a2))
      end
      when a0sym == "try*" then do
        res = eval(word(astval, 2), env_idx)
        if words(astval) < 3 then return res
        if res == "ERR" then do
          if word(err, 1) == "__MAL_EXCEPTION__" then
            errobj = word(err, 2)
          else
            errobj = new_string(err)
          catchlst = obj_val(word(astval, 3))
          catch_env_idx = new_env(env_idx, new_list(word(catchlst, 2)), new_list(errobj))
          err = ""
          return eval(word(catchlst, 3), catch_env_idx)
        end
        else
          return res
      end
      when a0sym == "do" then do
        do i=2 to (words(astval) - 1)
          res = eval(word(astval, i), env_idx)
          if res == "ERR" then return "ERR"
        end
        ast = word(astval, words(astval))
        /* TCO */
      end
      when a0sym == "if" then do
        condval = eval(word(astval, 2), env_idx)
        if false?(condval) | nil?(condval) then
          if words(astval) >= 4 then
            ast = word(astval, 4)
          else
            return new_nil()
        else
          ast = word(astval, 3)
        /* TCO */
      end
      when a0sym == "fn*" then return new_func(word(astval, 3), env_idx, word(astval, 2))
      otherwise
       f = eval(a0, env_idx)
       if f == "ERR" then return "ERR"

       if func_macro?(f) then do
         call_args = mal_rest(ast)
         mac_env_idx = new_env(func_env_idx(f), func_binds(f), call_args)
         ast = eval(func_body_ast(f), mac_env_idx)
         /* TCO */
       end
       else do

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
            env_idx = new_env(func_env_idx(f), func_binds(f), call_args)
            ast = func_body_ast(f)
            /* TCO */
          end
          otherwise
            err = "Unsupported function object type: " || obj_type(f)
            return "ERR"
          end
       end
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

mal_eval: procedure expose values. env. err /* mal_eval(ast) */
  ast = arg(1)
  if ast == "ERR" then return "ERR"
  return eval(arg(1), 1) /* repl_env_idx is always 1 because it's the first env */

build_args_list: procedure expose values. command_line_args. /* build_args_list() */
  seq = ""
  do i=2 to command_line_args.0
    s = new_string(command_line_args.i)
    if i == 1 then
      seq = s
    else
      seq = seq || " " || s
  end
  return new_list(seq)

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
  x = env_set(repl_env_idx, "eval", new_nativefn("mal_eval"))
  x = env_set(repl_env_idx, "*ARGV*", build_args_list())

  /* core.mal: defined using the language itself */
  x = re("(def! not (fn* (a) (if a false true)))")
  x = re('(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))')
  x = re("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw " || '"' || "odd number of forms to cond" || '"' || ")) (cons 'cond (rest (rest xs)))))))");

  err = ""
  if command_line_args.0 > 0 then do
    x = re('(load-file "' || command_line_args.1 || '")')
    return
  end

  do while lines() > 0 /* 1 == 1 */
    input_line = readline('user> ')
    if length(input_line) > 0 then do
      res = rep(input_line)
      if res == "ERR" then do
        if word(err, 1) == "__MAL_EXCEPTION__" then
          errstr = pr_str(word(err, 2), 0)
        else
          errstr = err
        call lineout , "Error: " || errstr
        err = ""
      end
      else
        call lineout , res
    end
  end
