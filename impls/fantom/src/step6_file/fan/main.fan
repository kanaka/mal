using mallib

class Main
{
  static MalVal READ(Str s)
  {
    return Reader.read_str(s)
  }

  static MalVal eval_ast(MalVal ast, MalEnv env)
  {
    switch (ast.typeof)
    {
      case MalSymbol#:
        return env.get(ast)
      case MalList#:
        newElements := (ast as MalList).value.map |MalVal v -> MalVal| { EVAL(v, env) }
        return MalList(newElements)
      case MalVector#:
        newElements := (ast as MalVector).value.map |MalVal v -> MalVal| { EVAL(v, env) }
        return MalVector(newElements)
      case MalHashMap#:
        newElements := (ast as MalHashMap).value.map |MalVal v -> MalVal| { EVAL(v, env) }
        return MalHashMap.fromMap(newElements)
      default:
        return ast
    }
  }

  static MalVal EVAL(MalVal ast, MalEnv env)
  {
    while (true)
    {
      if (!(ast is MalList)) return eval_ast(ast, env)
      astList := ast as MalList
      if (astList.isEmpty) return ast
      switch ((astList[0] as MalSymbol)?.value)
      {
        case "def!":
          return env.set(astList[1], EVAL(astList[2], env))
        case "let*":
          let_env := MalEnv(env)
          varList := astList[1] as MalSeq
          for (i := 0; i < varList.count; i += 2)
            let_env.set(varList[i], EVAL(varList[i + 1], let_env))
          env = let_env
          ast = astList[2]
          // TCO
        case "do":
          eval_ast(MalList(astList[1..-2]), env)
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
          evaled_ast := eval_ast(ast, env) as MalList
          switch (evaled_ast[0].typeof)
          {
            case MalUserFunc#:
              user_fn := evaled_ast[0] as MalUserFunc
              ast = user_fn.ast
              env = user_fn.genEnv(evaled_ast.drop(1))
              // TCO
            case MalFunc#:
              return (evaled_ast[0] as MalFunc).call(evaled_ast[1..-1])
            default:
              throw Err("Unknown type")
          }
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

  static Void main(Str[] args)
  {
    repl_env := MalEnv()
    // core.fan: defined using Fantom
    Core.ns.each |MalFunc V, Str K| { repl_env.set(MalSymbol(K), V) }
    repl_env.set(MalSymbol("eval"), MalFunc { EVAL(it[0], repl_env) })
    repl_env.set(MalSymbol("*ARGV*"), MalList((args.isEmpty ? args : args[1..-1]).map { MalString.make(it) }))

    // core.mal: defined using the language itself
    REP("(def! not (fn* (a) (if a false true)))", repl_env)
    REP("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))", repl_env)

    if (!args.isEmpty)
    {
      REP("(load-file \"${args[0]}\")", repl_env)
      return
    }

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
