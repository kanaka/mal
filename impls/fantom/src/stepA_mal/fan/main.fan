using mallib

class Main
{

  static MalList qq_loop(MalVal elt, MalList acc)
  {
    lst := elt as MalList
    if (lst?.count == 2 && (lst[0] as MalSymbol)?.value == "splice-unquote")
      return MalList(MalVal[MalSymbol("concat"), lst[1], acc])
    else
      return MalList(MalVal[MalSymbol("cons"), quasiquote(elt), acc])
  }

  static MalList qq_foldr(MalSeq xs)
  {
    acc := MalList([,])
    for (i:=xs.count-1; 0<=i; i-=1)
      acc = qq_loop(xs[i], acc)
    return acc
  }

  static MalVal quasiquote(MalVal ast)
  {
    switch (ast.typeof)
    {
      case MalList#:
        lst := ast as MalList
        if (lst.count == 2 && (lst[0] as MalSymbol)?.value == "unquote")
          return  lst[1]
        else
          return qq_foldr((MalSeq)ast)
      case MalVector#:
        return MalList(MalVal[MalSymbol("vec"), qq_foldr((MalSeq)ast)])
      case MalSymbol#:
        return MalList(MalVal[MalSymbol("quote"), ast])
      case MalHashMap#:
        return MalList(MalVal[MalSymbol("quote"), ast])
      default:
        return ast
    }
  }

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
            case "quote":
              return astList[1]
            case "quasiquote":
              ast = quasiquote(astList[1])
              // TCO
            case "defmacro!":
              f := (EVAL(astList[2], env) as MalUserFunc).dup
              f.isMacro = true
              return env.set(astList[1], f)
            case "try*":
              if (astList.count < 3)
                return EVAL(astList[1], env)
              MalVal exc := MalNil.INSTANCE
              try
                return EVAL(astList[1], env)
              catch (MalException e)
                exc = e.getValue
              catch (Err e)
                exc = MalString.make(e.msg)
              catchClause := astList[2] as MalList
              return EVAL(catchClause[2], MalEnv(env, MalList([catchClause[1]]), MalList([exc])))
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
              args := astList.value[1..-1]
              switch (f.typeof)
              {
                case MalUserFunc#:
                  user_fn := f as MalUserFunc
                  if (user_fn.isMacro) {
                    ast = user_fn.call(args)
                    continue // TCO
                  }
                  args = args.map |MalVal v -> MalVal| { EVAL(v, env) }
                  ast = user_fn.ast
                  env = user_fn.genEnv(MalList(args))
                  // TCO
                case MalFunc#:
                  malfunc := f as MalFunc
                  args = args.map |MalVal v -> MalVal| { EVAL(v, env) }
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

  static Void main(Str[] args)
  {
    repl_env := MalEnv()
    // core.fan: defined using Fantom
    Core.ns.each |MalFunc V, Str K| { repl_env.set(MalSymbol(K), V) }
    repl_env.set(MalSymbol("eval"), MalFunc { EVAL(it[0], repl_env) })
    repl_env.set(MalSymbol("*ARGV*"), MalList((args.isEmpty ? args : args[1..-1]).map { MalString.make(it) }))

    // core.mal: defined using the language itself
    REP("(def! *host-language* \"fantom\")", repl_env)
    REP("(def! not (fn* (a) (if a false true)))", repl_env)
    REP("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))", repl_env)
    REP("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))", repl_env)

    if (!args.isEmpty)
    {
      REP("(load-file \"${args[0]}\")", repl_env)
      return
    }

    REP("(println (str \"Mal [\" *host-language* \"]\"))", repl_env)
    while (true) {
      line := Env.cur.prompt("user> ")
      if (line == null) break
      if (line.isSpace) continue
      try
        echo(REP(line, repl_env))
      catch (MalException e)
        echo("Error: ${e.serializedValue}")
      catch (Err e)
        echo("Error: $e.msg")
    }
  }
}
