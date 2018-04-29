using mallib

class Main
{
  static MalVal READ(Str s)
  {
    return Reader.read_str(s)
  }

  static MalVal eval_ast(MalVal ast, Str:MalFunc env)
  {
    switch (ast.typeof)
    {
      case MalSymbol#:
        varName := (ast as MalSymbol).value
        varVal := env[varName] ?: throw Err("'$varName' not found")
        return (MalVal)varVal
      case MalList#:
        newElements := (ast as MalList).value.map { EVAL(it, env) }
        return MalList(newElements)
      case MalVector#:
        newElements := (ast as MalVector).value.map { EVAL(it, env) }
        return MalVector(newElements)
      case MalHashMap#:
        newElements := (ast as MalHashMap).value.map |MalVal v -> MalVal| { return EVAL(v, env) }
        return MalHashMap.fromMap(newElements)
      default:
        return ast
    }
  }

  static MalVal EVAL(MalVal ast, Str:MalFunc env)
  {
    if (!(ast is MalList)) return eval_ast(ast, env)
    astList := ast as MalList
    if (astList.isEmpty) return ast
    evaled_ast := eval_ast(ast, env) as MalList
    f := evaled_ast[0] as MalFunc
    return f.call(evaled_ast[1..-1])
  }

  static Str PRINT(MalVal exp)
  {
    return exp.toString(true)
  }

  static Str REP(Str s, Str:MalFunc env)
  {
    return PRINT(EVAL(READ(s), env))
  }

  static Void main()
  {
    env := [
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
        echo(REP(line, env))
      catch (Err e)
        echo("Error: $e.msg")
    }
  }
}
