using mallib

class Main
{
  static MalVal READ(Str s)
  {
    return Reader.read_str(s)
  }

  static Void debug_eval(MalVal ast, MalEnv env)
  {
    value := env.get("DEBUG-EVAL")
    if ((value != null) && !(value is MalFalseyVal))
        echo("EVAL: ${PRINT(ast)}")
  }

  static MalVal EVAL(MalVal ast, MalEnv env)
  {
      debug_eval(ast, env)
      switch (ast.typeof)
      {
        case MalSymbol#:
          varName := (ast as MalSymbol).value
          return env.get(varName) ?: throw Err("'$varName' not found")
        case MalVector#:
          newElements := (ast as MalVector).value.map |MalVal v -> MalVal| { EVAL(v, env) }
          return MalVector(newElements)
        case MalHashMap#:
          newElements := (ast as MalHashMap).value.map |MalVal v -> MalVal| { EVAL(v, env) }
          return MalHashMap.fromMap(newElements)
        case MalList#:
          astList := ast as MalList
          if (astList.isEmpty) return ast
          switch ((astList[0] as MalSymbol)?.value)
          {
            case "def!":
              value := EVAL(astList[2], env)
              return env.set(astList[1], value)
            case "let*":
              let_env := MalEnv(env)
              varList := astList[1] as MalSeq
              for (i := 0; i < varList.count; i += 2)
                let_env.set(varList[i], EVAL(varList[i + 1], let_env))
              return EVAL(astList[2], let_env)
            default:
              f := EVAL(astList[0], env)
              args := astList.value[1..-1].map |MalVal v -> MalVal| { EVAL(v, env) }

                  malfunc := f as MalFunc
                  return malfunc.call(args)

          }
        default:
          return ast
      }
  }

  static Str PRINT(MalVal exp)
  {
    return exp.toString(true)
  }

  static Str REP(Str s, MalEnv env)
  {
    return PRINT(EVAL(READ(s), env))
  }

  static Void main()
  {
    repl_env := MalEnv()
    repl_env.set(MalSymbol("+"), MalFunc { MalInteger((it[0] as MalInteger).value + (it[1] as MalInteger).value) })
    repl_env.set(MalSymbol("-"), MalFunc { MalInteger((it[0] as MalInteger).value - (it[1] as MalInteger).value) })
    repl_env.set(MalSymbol("*"), MalFunc { MalInteger((it[0] as MalInteger).value * (it[1] as MalInteger).value) })
    repl_env.set(MalSymbol("/"), MalFunc { MalInteger((it[0] as MalInteger).value / (it[1] as MalInteger).value) })
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
