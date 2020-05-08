import .Readline;

string READ(string str)
{
  return str;
}

string EVAL(string ast, string env)
{
  return ast;
}

string PRINT(string exp)
{
  return exp;
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
    write(({ rep(line), "\n" }));
  }
  write("\n");
  return 0;
}
