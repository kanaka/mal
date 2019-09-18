import .Printer;
import .Reader;
import .Readline;
import .Types;

Val READ(string str)
{
  return read_str(str);
}

Val EVAL(Val ast, string env)
{
  return ast;
}

string PRINT(Val exp)
{
  return pr_str(exp, true);
}

string rep(string str)
{
  return PRINT(EVAL(READ(str), ""));
}

int main()
{
  while(1)
  {
    string line = readline("user> ");
    if(!line) break;
    if(strlen(line) == 0) continue;
    if(mixed err = catch { write(({ rep(line), "\n" })); } )
    {
      if(arrayp(err)) err = err[0];
      write(({ "Error: ", err, "\n" }));
    }
  }
  write("\n");
  return 0;
}
