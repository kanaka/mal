func READ(str)
{
  return str
}

func EVAL(exp, env)
{
  return exp
}

func PRINT(exp)
{
  return exp
}

func REP(str)
{
  return PRINT(EVAL(READ(str), ""))
}

func main(void)
{
  stdin_file = open("/dev/stdin", "r")
  while (1) {
    write, format="%s", "user> "
    line = rdline(stdin_file, prompt="")
    if (!line) break
    if (strlen(line) > 0) write, format="%s\n", REP(line)
  }
  write, ""
}

main;
