set_path, get_env("YORICK_MAL_PATH") + ":" + get_path()
require, "reader.i"
require, "printer.i"

func READ(str)
{
  return read_str(str)
}

func EVAL(exp, env)
{
  if (structof(exp) == MalError) return exp
  return exp
}

func PRINT(exp)
{
  if (structof(exp) == MalError) return exp
  return pr_str(exp, 1)
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
    if (strlen(line) > 0) {
      result = REP(line)
      if (structof(result) == MalError) write, format="Error: %s\n", result.message
      else write, format="%s\n", result
    }
  }
  write, ""
}

main;
