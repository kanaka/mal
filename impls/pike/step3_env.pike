import .Env;
import .Printer;
import .Reader;
import .Readline;
import .Types;

Val READ(string str)
{
  return read_str(str);
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
        return EVAL(ast.data[2], let_env);
    }
  }
  Val evaled_ast = eval_ast(ast, env);
  function f = evaled_ast.data[0];
  return f(@evaled_ast.data[1..]);
}

string PRINT(Val exp)
{
  return pr_str(exp, true);
}

string rep(string str, Env env)
{
  return PRINT(EVAL(READ(str), env));
}

int main()
{
  Env repl_env = Env(0);
  repl_env.set(Symbol("+"), lambda(Val a, Val b) { return Number(a.value + b.value); });
  repl_env.set(Symbol("-"), lambda(Val a, Val b) { return Number(a.value - b.value); });
  repl_env.set(Symbol("*"), lambda(Val a, Val b) { return Number(a.value * b.value); });
  repl_env.set(Symbol("/"), lambda(Val a, Val b) { return Number(a.value / b.value); });
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
