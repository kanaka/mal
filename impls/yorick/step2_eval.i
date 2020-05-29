set_path, get_env("YORICK_MAL_PATH") + ":" + get_path()
require, "reader.i"
require, "printer.i"
require, "core.i"

func READ(str)
{
  return read_str(str)
}

func eval_ast(ast, env)
{
  type = structof(ast)
  if (type == MalSymbol) {
    val = h_get(env, ast.val)
    if (is_void(val)) return MalError(message=("'" + ast.val + "' not found"))
    return val
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
  if (numberof(*ast.val) == 0) return ast
  el = eval_ast(ast, env)
  if (structof(el) == MalError) return el
  seq = *el.val
  args = (numberof(seq) > 1) ? seq(2:) : []
  return call_core_fn(seq(1)->val, args)
}

func PRINT(exp)
{
  if (structof(exp) == MalError) return exp
  return pr_str(exp, 1)
}

func REP(str, env)
{
  return PRINT(EVAL(READ(str), env))
}

func main(void)
{
  repl_env = h_new()
  h_set, repl_env, "+", MalNativeFunction(val="+")
  h_set, repl_env, "-", MalNativeFunction(val="-")
  h_set, repl_env, "*", MalNativeFunction(val="*")
  h_set, repl_env, "/", MalNativeFunction(val="/")

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
