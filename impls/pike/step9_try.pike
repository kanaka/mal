import .Env;
import .Printer;
import .Reader;
import .Readline;
import .Types;

Val READ(string str)
{
  return read_str(str);
}

bool starts_with(Val ast, string sym)
{
  return ast.mal_type == MALTYPE_LIST &&
    !ast.emptyp() &&
    ast.data[0].mal_type == MALTYPE_SYMBOL &&
    ast.data[0].value == sym;
}

Val quasiquote_list(array(Val) elts)
{
  Val acc = List(({ }));
  for(int i=sizeof(elts)-1; 0<=i; i-=1)
  {
    Val elt = elts[i];
    if(starts_with(elt, "splice-unquote"))
      acc = List(({ Symbol("concat"), elt.data[1], acc }));
    else
      acc = List(({ Symbol("cons"), quasiquote(elt), acc }));
  }
  return acc;
}

Val quasiquote(Val ast)
{
  switch(ast.mal_type)
  {
    case MALTYPE_LIST:
      if(starts_with(ast, "unquote"))
        return ast.data[1];
      else
        return quasiquote_list(ast.data);
    case MALTYPE_VECTOR:
      return List(({ Symbol("vec"), quasiquote_list(ast.data) }));
  case MALTYPE_SYMBOL:
  case MALTYPE_MAP:
      return List(({ Symbol("quote"), ast }));
  default:
      return ast;
  }
}

Val EVAL(Val ast, Env env)
{
  while(true)
  {

  Val dbgeval = env.get("DEBUG-EVAL");
  if(dbgeval && dbgeval.mal_type != MALTYPE_FALSE
     && dbgeval.mal_type != MALTYPE_NIL)
    write(({ "EVAL: ", PRINT(ast), "\n" }));

  switch(ast.mal_type)
  {
    case MALTYPE_SYMBOL:
      Val key = ast.value;
      Val val = env.get(ast.value);
      if(!val) throw("'" + key + "' not found");
      return val;
    case MALTYPE_LIST:
      break;
    case MALTYPE_VECTOR:
      return Vector(map(ast.data, lambda(Val e) { return EVAL(e, env); }));
    case MALTYPE_MAP:
      array(Val) elements = ({ });
      foreach(ast.data; Val k; Val v)
      {
        elements += ({ k, EVAL(v, env) });
      }
      return Map(elements);
    default:
      return ast;
    }

    if(ast.emptyp()) return ast;
    if(ast.data[0].mal_type == MALTYPE_SYMBOL) {
      switch(ast.data[0].value)
      {
        case "def!":
          return env.set(ast.data[1], EVAL(ast.data[2], env));
        case "let*":
          Env let_env = Env(env);
          Val ast1 = ast.data[1];
          for(int i = 0; i < sizeof(ast1.data); i += 2)
          {
            let_env.set(ast1.data[i], EVAL(ast1.data[i + 1], let_env));
          }
          env = let_env;
          ast = ast.data[2];
          continue; // TCO
        case "quote":
          return ast.data[1];
        case "quasiquote":
          ast = quasiquote(ast.data[1]);
          continue; // TCO
        case "defmacro!":
          Val macro = EVAL(ast.data[2], env).clone_as_macro();
          return env.set(ast.data[1], macro);
        case "try*":
          if(ast.count() < 3) return EVAL(ast.data[1], env);
          if(mixed err = catch { return EVAL(ast.data[1], env); } )
          {
            Val err_val;
            if(objectp(err)) err_val = err;
            else if(stringp(err)) err_val = String(err);
            else if(arrayp(err)) err_val = String(err[0]);
            Val catch_clause = ast.data[2];
            Env catch_env = Env(env);
            catch_env.set(catch_clause.data[1], err_val);
            return EVAL(catch_clause.data[2], catch_env);
          }
        case "do":
          Val result;
          foreach(ast.data[1..(sizeof(ast.data) - 2)], Val element)
          {
            result = EVAL(element, env);
          }
          ast = ast.data[-1];
          continue; // TCO
        case "if":
          Val cond = EVAL(ast.data[1], env);
          if(cond.mal_type == MALTYPE_FALSE || cond.mal_type == MALTYPE_NIL)
          {
            if(sizeof(ast.data) > 3)
              ast = ast.data[3];
            else
              return MAL_NIL;
          }
          else
            ast = ast.data[2];
          continue; // TCO
        case "fn*":
          return Fn(ast.data[2], ast.data[1], env,
                    lambda(Val ... a) { return EVAL(ast.data[2], Env(env, ast.data[1], List(a))); });
      }
    }
    Val f = EVAL(ast.data[0], env);
    array(Val) args = ast.data[1..];
    switch(f.mal_type)
    {
      case MALTYPE_BUILTINFN:
        return f(@map(args, lambda(Val e) { return EVAL(e, env);}));
      case MALTYPE_FN:
        if(f.macro)
        {
          ast = f(@args);
          continue; // TCO
        }
        ast = f.ast;
        env = Env(f.env, f.params, List(map(args, lambda(Val e) { return EVAL(e, env);})));
        continue; // TCO
      default:
        throw("Unknown function type");
    }
  }
}

string PRINT(Val exp)
{
  return pr_str(exp, true);
}

string rep(string str, Env env)
{
  return PRINT(EVAL(READ(str), env));
}

int main(int argc, array argv)
{
  Env repl_env = Env(0);
  foreach(.Core.NS(); Val k; Val v) repl_env.set(k, v);
  repl_env.set(Symbol("eval"), BuiltinFn("eval", lambda(Val a) { return EVAL(a, repl_env); }));
  repl_env.set(Symbol("*ARGV*"), List(map(argv[2..], String)));
  rep("(def! not (fn* (a) (if a false true)))", repl_env);
  rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))", repl_env);
  rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))", repl_env);
  if(argc >= 2)
  {
    rep("(load-file \"" + argv[1] + "\")", repl_env);
    return 0;
  }
  while(1)
  {
    string line = readline("user> ");
    if(!line) break;
    if(strlen(line) == 0) continue;
    if(mixed err = catch { write(({ rep(line, repl_env), "\n" })); } )
    {
      if(objectp(err))
      {
        err = err.to_string(true);
      }
      else if(arrayp(err))
      {
        err = err[0];
      }
      write(({ "Error: ", err, "\n" }));
    }
  }
  write("\n");
  return 0;
}
