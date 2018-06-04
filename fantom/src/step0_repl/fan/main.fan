class Main
{
  static Str READ(Str s)
  {
    return s
  }

  static Str EVAL(Str ast, Str env)
  {
    return ast
  }

  static Str PRINT(Str exp)
  {
    return exp
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
      echo(REP(line, ""))
    }
  }
}
