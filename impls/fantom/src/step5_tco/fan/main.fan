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
    while (true)
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
              env = let_env
              ast = astList[2]
              // TCO
            case "do":
              for (i:=1; i<astList.count-1; i+=1)
                EVAL(astList[i], env);
              ast = astList[-1]
              // TCO
            case "if":
              if (EVAL(astList[1], env) is MalFalseyVal)
                ast = astList.count > 3 ? astList[3] : MalNil.INSTANCE
              else
                ast = astList[2]
              // TCO
            case "fn*":
              f := |MalVal[] a -> MalVal|
              {
                return EVAL(astList[2], MalEnv(env, (astList[1] as MalSeq), MalList(a)))
              }
              return MalUserFunc(astList[2], env, (MalSeq)astList[1], f)
            default:
              f := EVAL(astList[0], env)
              args := astList.value[1..-1].map |MalVal v -> MalVal| { EVAL(v, env) }
              switch (f.typeof)
              {
                case MalUserFunc#:
                  user_fn := f as MalUserFunc
                  ast = user_fn.ast
                  env = user_fn.genEnv(MalList(args))
                  // TCO
                case MalFunc#:
                  malfunc := f as MalFunc
                  return malfunc.call(args)
                default:
                  throw Err("Unknown type")
              }
          }
        default:
          return ast
      }
    }
    return MalNil.INSTANCE // never reached
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
    // core.fan: defined using Fantom
    Core.ns.each |MalFunc V, Str K| { repl_env.set(MalSymbol(K), V) }

    // core.mal: defined using the language itself
    REP("(def! not (fn* (a) (if a false true)))", repl_env)

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
