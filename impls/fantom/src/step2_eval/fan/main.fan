using mallib

class Main
{
  static MalVal READ(Str s)
  {
    return Reader.read_str(s)
  }

  static MalVal EVAL(MalVal ast, Str:MalVal env)
  {
      switch (ast.typeof)
      {
        case MalSymbol#:
          varName := (ast as MalSymbol).value
          return env[varName] ?: throw Err("'$varName' not found")
        case MalVector#:
          newElements := (ast as MalVector).value.map |MalVal v -> MalVal| { EVAL(v, env) }
          return MalVector(newElements)
        case MalHashMap#:
          newElements := (ast as MalHashMap).value.map |MalVal v -> MalVal| { EVAL(v, env) }
          return MalHashMap.fromMap(newElements)
        case MalList#:
          astList := ast as MalList
          if (astList.isEmpty) return ast

              f := EVAL(astList[0], env)
              args := astList.value[1..-1].map |MalVal v -> MalVal| { EVAL(v, env) }

                  malfunc := f as MalFunc
                  return malfunc.call(args)

        default:
          return ast
      }
  }

  static Str PRINT(MalVal exp)
  {
    return exp.toString(true)
  }

  static Str REP(Str s, Str:MalVal env)
  {
    return PRINT(EVAL(READ(s), env))
  }

  static Void main()
  {
    repl_env := [
      "+": MalFunc { MalInteger((it[0] as MalInteger).value + (it[1] as MalInteger).value) },
      "-": MalFunc { MalInteger((it[0] as MalInteger).value - (it[1] as MalInteger).value) },
      "*": MalFunc { MalInteger((it[0] as MalInteger).value * (it[1] as MalInteger).value) },
      "/": MalFunc { MalInteger((it[0] as MalInteger).value / (it[1] as MalInteger).value) }
    ]
    while (true) {
      line := Env.cur.prompt("user> ")
      if (line == null) break
      if (line.isSpace) continue
      try
        echo(REP(line, repl_env))
      catch (Err e)
        echo("Error: $e.msg")
    }
  }
}
