import .Env;
import .Printer;
import .Reader;
import .Readline;
import .Types;

Val READ(string str)
{
  return read_str(str);
}

bool is_pair(Val e)
{
  return e.is_sequence && !e.emptyp();
}

Val quasiquote(Val ast)
{
  if(!is_pair(ast)) return List(({ Symbol("quote"), ast }));
  Val ast0 = ast.data[0];
  if(ast0.mal_type == MALTYPE_SYMBOL && ast0.value == "unquote") return ast.data[1];
  if(is_pair(ast0) && ast0.data[0].mal_type == MALTYPE_SYMBOL && ast0.data[0].value == "splice-unquote")
  {
    return List(({ Symbol("concat"), ast0.data[1], quasiquote(ast.rest()) }));
  }
  return List(({ Symbol("cons"), quasiquote(ast0), quasiquote(ast.rest()) }));
}

Val eval_ast(Val ast, Env env)
{
  switch(ast.mal_type)
  {
    case MALTYPE_SYMBOL:
      return env.get(ast);
    case MALTYPE_LIST:
      return List(map(ast.data, lambda(Val e) { return EVAL(e, env); }));
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
}

Val EVAL(Val ast, Env env)
{
  while(true)
  {
    if(ast.mal_type != MALTYPE_LIST) return eval_ast(ast, env);
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
    Val evaled_ast = eval_ast(ast, env);
    Val f = evaled_ast.data[0];
    switch(f.mal_type)
    {
      case MALTYPE_BUILTINFN:
        return f(@evaled_ast.data[1..]);
      case MALTYPE_FN:
        ast = f.ast;
        env = Env(f.env, f.params, List(evaled_ast.data[1..]));
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
      if(arrayp(err)) err = err[0];
      write(({ "Error: ", err, "\n" }));
    }
  }
  write("\n");
  return 0;
}
