set_path, get_env("YORICK_MAL_PATH") + ":" + get_path()
require, "reader.i"
require, "printer.i"
require, "core.i"
require, "env.i"

func READ(str)
{
  return read_str(str)
}

func starts_with(seq, sym)
{
  return numberof(seq) == 2 && structof(*seq(1)) == MalSymbol && seq(1)->val == sym
}

func quasiquote_loop(seq)
{
  acc = MalList(val=&[])
  for (i=numberof(seq); 0<i; --i) {
    elt = *seq(i)
    if (structof(elt) == MalList && starts_with(*elt.val, "splice-unquote")) {
      acc = MalList(val=&[&MalSymbol(val="concat"), (*elt.val)(2), &acc])
    } else {
      acc = MalList(val=&[&MalSymbol(val="cons"), &quasiquote(elt), &acc])
    }
  }
  return acc
}

func quasiquote(ast)
{
  type = structof(ast)
  if (type == MalList) {
    seq = *ast.val
    if (starts_with(seq, "unquote")) {
      return *seq(2)
    } else {
      return quasiquote_loop(seq)
    }
  } else if (type == MalVector) {
    return MalList(val=&[&MalSymbol(val="vec"), &quasiquote_loop(*ast.val)])
  } else if (type == MalHashmap || type == MalSymbol) {
    return MalList(val=&[&MalSymbol(val="quote"), &ast])
  } else {
    return ast
  }
}

func eval_ast(ast, env)
{
    seq = *(ast.val)
    if (numberof(seq) == 0) return ast
    res = array(pointer, numberof(seq))
    for (i = 1; i <= numberof(seq); ++i) {
      e = EVAL(*seq(i), env)
      if (structof(e) == MalError) return e
      res(i) = &e
    }
    return MalList(val=&res)
}

func EVAL(ast, env)
{
  while (1) {
    dbgeval = structof(env_get(env, "DEBUG-EVAL"))
    if ((dbgeval != MalError) && (dbgeval != MalNil) && (dbgeval != MalFalse)) {
       write, format="EVAL: %s\n", pr_str(ast, 1)
    }
    // Process non-list types (todo: indent right)
  type = structof(ast)
  if (type == MalSymbol) {
    return env_get(env, ast.val)
  } else if (type == MalList) {
    // Proceed after this switch.
  } else if (type == MalVector) {
    seq = *(ast.val)
    if (numberof(seq) == 0) return ast
    res = array(pointer, numberof(seq))
    for (i = 1; i <= numberof(seq); ++i) {
      e = EVAL(*seq(i), env)
      if (structof(e) == MalError) return e
      res(i) = &e
    }
    return MalVector(val=&res)
  } else if (type == MalHashmap) {
    h = *(ast.val)
    if (numberof(*h.keys) == 0) return ast
    res = hash_new()
    for (i = 1; i <= numberof(*h.keys); ++i) {
      new_val = EVAL(*((*h.vals)(i)), env)
      if (structof(new_val) == MalError) return new_val
      hash_set, res, (*h.keys)(i), new_val
    }
    return MalHashmap(val=&res)
  } else return ast
    // The else branch includes MalError. Now ast is a list.
    lst = *ast.val
    if (numberof(lst) == 0) return ast
    a1 = lst(1)->val
    if (a1 == "def!") {
      new_value = EVAL(*lst(3), env)
      if (structof(new_value) == MalError) return new_value
      return env_set(env, lst(2)->val, new_value)
    } else if (a1 == "let*") {
      let_env = env_new(&env)
      args_lst = *(lst(2)->val)
      for (i = 1; i <= numberof(args_lst); i += 2) {
        var_name = args_lst(i)->val
        var_value = EVAL(*args_lst(i + 1), let_env)
        if (structof(var_value) == MalError) return var_value
        env_set, let_env, var_name, var_value
      }
      ast = *lst(3)
      env = let_env
      // TCO
    } else if (a1 == "quote") {
      return *lst(2)
    } else if (a1 == "quasiquote") {
      ast = quasiquote(*lst(2)) // TCO
    } else if (a1 == "do") {
      for (i = 2; i < numberof(lst); ++i) {
        ret = EVAL(*lst(i), env)
        if (structof(ret) == MalError) return ret
      }
      ast = *lst(numberof(lst))
      // TCO
    } else if (a1 == "if") {
      cond_val = EVAL(*lst(2), env)
      if (structof(cond_val) == MalError) return cond_val
      if ((structof(cond_val) == MalNil) || (structof(cond_val) == MalFalse)) {
        if (numberof(lst) > 3) {
          ast = *lst(4)
        } else {
          return MAL_NIL
        }
      } else {
        ast = *lst(3)
      }
      // TCO
    } else if (a1 == "fn*") {
      return MalFunction(env=&env, binds=lst(2)->val, ast=lst(3))
    } else {
      el = eval_ast(ast, env)
      if (structof(el) == MalError) return el
      seq = *el.val
      if (structof(*seq(1)) == MalNativeFunction) {
        args = (numberof(seq) > 1) ? seq(2:) : []
        return call_core_fn(seq(1)->val, args)
      } else if (structof(*seq(1)) == MalFunction) {
        fn = *seq(1)
        exprs = numberof(seq) > 1 ? seq(2:) : []
        fn_env = env_new(fn.env, binds=*fn.binds, exprs=exprs)
        ast = *fn.ast
        env = fn_env
        // TCO
      } else {
        return MalError(message="Unknown function type")
      }
    }
  }
}

func PRINT(exp)
{
  if (structof(exp) == MalError) return exp
  return pr_str(exp, 1)
}

func RE(str, env)
{
  return EVAL(READ(str), env)
}

func REP(str, env)
{
  return PRINT(EVAL(READ(str), env))
}

func get_command_line(void)
// Force quiet mode (-q) to prevent Yorick from printing its banner
{
  argv = get_argv()
  return numberof(argv) > 1 ? grow([argv(1), "-q"], argv(2:)) : [argv(1), "-q"]
}

func prepare_argv_list(args)
{
  if (numberof(args) <= 1) return MalList(val=&[])
  str_lst = array(pointer, numberof(args) - 1)
  for (i = 2; i <= numberof(args); ++i) {
    str_lst(i - 1) = &MalString(val=args(i))
  }
  return MalList(val=&str_lst)
}

repl_env = nil

func main(void)
{
  extern repl_env
  repl_env = env_new(pointer(0))

  // core.i: defined using Yorick
  core_symbols = h_keys(core_ns)
  for (i = 1; i <= numberof(core_symbols); ++i) {
    env_set, repl_env, core_symbols(i), MalNativeFunction(val=core_symbols(i))
  }
  command_line_args = process_argv()
  env_set, repl_env, "*ARGV*", prepare_argv_list(command_line_args)

  // core.mal: defined using the language itself
  RE, "(def! not (fn* (a) (if a false true)))", repl_env
  RE, "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\\nnil)\")))))", repl_env

  if (numberof(command_line_args) > 0) {
    RE, "(load-file \"" + command_line_args(1) + "\")", repl_env
    return 0
  }

  stdin_file = open("/dev/stdin", "r")
  while (1) {
    write, format="%s", "user> "
    line = rdline(stdin_file, prompt="")
    if (!line) break
    if (strlen(line) > 0) {
      result = REP(line, repl_env)
      if (structof(result) == MalError) write, format="Error: %s\n", result.message
      else write, format="%s\n", result
    }
  }
  write, ""
}

main;
