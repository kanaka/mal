import .Printer;
import .Reader;
import .Readline;
import .Types;

Val READ(string str)
{
  return read_str(str);
}

Val EVAL(Val ast, mapping(string:function) env)
{
  // write(({ "EVAL: ", PRINT(ast), "\n" }));

  switch(ast.mal_type)
  {
    case MALTYPE_SYMBOL:
      function f = env[ast.value];
      if(!f) throw("'" + ast.value + "' not found");
      return f;
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
  Val f = EVAL(ast.data[0], env);
  array(Val) args = ast.data[1..];
  args = map(args, lambda(Val e) { return EVAL(e, env);});
  return f(@args);
}

string PRINT(Val exp)
{
  return pr_str(exp, true);
}

string rep(string str, mapping(string:function) env)
{
  return PRINT(EVAL(READ(str), env));
}

int main()
{
  mapping(string:function) repl_env = ([
    "+": lambda(Val a, Val b) { return Number(a.value + b.value); },
    "-": lambda(Val a, Val b) { return Number(a.value - b.value); },
    "*": lambda(Val a, Val b) { return Number(a.value * b.value); },
    "/": lambda(Val a, Val b) { return Number(a.value / b.value); }
  ]);
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
