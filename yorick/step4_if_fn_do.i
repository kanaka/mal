set_path, get_env("YORICK_MAL_PATH") + ":" + get_path()
require, "reader.i"
require, "printer.i"
require, "core.i"
require, "env.i"

func READ(str)
{
  return read_str(str)
}

func eval_ast(ast, env)
{
  type = structof(ast)
  if (type == MalSymbol) {
    return env_get(env, ast.val)
  } else if (type == MalList) {
    seq = *(ast.val)
    if (numberof(seq) == 0) return ast
    res = array(pointer, numberof(seq))
    for (i = 1; i <= numberof(seq); ++i) {
      e = EVAL(*seq(i), env)
      if (structof(e) == MalError) return e
      res(i) = &e
    }
    return MalList(val=&res)
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
      new_key = EVAL(hashmap_key_to_obj((*h.keys)(i)), env)
      if (structof(new_key) == MalError) return new_key
      new_val = EVAL(*((*h.vals)(i)), env)
      if (structof(new_val) == MalError) return new_val
      hash_set, res, hashmap_obj_to_key(new_key), new_val
    }
    return MalHashmap(val=&res)
  } else return ast
}

func EVAL(ast, env)
{
  if (structof(ast) == MalError) return ast
  if (structof(ast) != MalList) return eval_ast(ast, env)
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
    return EVAL(*lst(3), let_env)
  } else if (a1 == "do") {
    ret = nil
    for (i = 2; i <= numberof(lst); ++i) {
      ret = EVAL(*lst(i), env)
      if (structof(ret) == MalError) return ret
    }
    return ret
  } else if (a1 == "if") {
    cond_val = EVAL(*lst(2), env)
    if (structof(cond_val) == MalError) return cond_val
    if ((structof(cond_val) == MalNil) || (structof(cond_val) == MalFalse)) {
      if (numberof(lst) > 3) {
        return EVAL(*lst(4), env)
      } else {
        return MAL_NIL
      }
    } else {
      return EVAL(*lst(3), env)
    }
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
      return EVAL(*fn.ast, fn_env)
    } else {
      return MalError(message="Unknown function type")
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

func main(void)
{
  repl_env = env_new(pointer(0))

  // core.i: defined using Yorick
  core_symbols = h_keys(core_ns)
  for (i = 1; i <= numberof(core_symbols); ++i) {
    env_set, repl_env, core_symbols(i), MalNativeFunction(val=core_symbols(i))
  }

  // core.mal: defined using the language itself
  RE, "(def! not (fn* (a) (if a false true)))", repl_env

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
