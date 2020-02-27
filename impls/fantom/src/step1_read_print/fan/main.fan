using mallib

class Main
{
  static MalVal READ(Str s)
  {
    return Reader.read_str(s)
  }

  static MalVal EVAL(MalVal ast, Str env)
  {
    return ast
  }

  static Str PRINT(MalVal exp)
  {
    return exp.toString(true)
  }

  static Str REP(Str s, Str env)
  {
    return PRINT(EVAL(READ(s), env))
  }

  static Void main()
  {
    while (true) {
      line := Env.cur.prompt("user> ")
      if (line == null) break
      if (line.isSpace) continue
      try
        echo(REP(line, ""))
      catch (Err e)
        echo("Error: $e.msg")
    }
  }
}
