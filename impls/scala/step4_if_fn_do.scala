import types.{MalList, _list, _list_Q, MalVector, MalHashMap,
              Func, MalFunction}
import env.Env

object step4_if_fn_do {
  // read
  def READ(str: String): Any = {
    reader.read_str(str)
  }

  // eval
  def EVAL(ast: Any, env: Env): Any = {

    if (env.find(Symbol("DEBUG-EVAL")) != null) {
      val dbgeval = env.get(Symbol("DEBUG-EVAL"))
      if (dbgeval != null && dbgeval != false) {
        println("EVAL: " + printer._pr_str(ast,true))
      }
    }

    ast match {
      case s : Symbol    => return env.get(s)
      case v: MalVector  => return v.map(EVAL(_, env))
      case l: MalList    => {}
      case m: MalHashMap => {
        return m.map{case (k,v) => (k, EVAL(v, env))}
      }
      case _             => return ast
    }

    // apply list
    ast.asInstanceOf[MalList].value match {
      case Nil => {
        return ast
      }
      case Symbol("def!") :: a1 :: a2 :: Nil => {
        return env.set(a1.asInstanceOf[Symbol], EVAL(a2, env))
      }
      case Symbol("let*") :: a1 :: a2 :: Nil => {
        val let_env = new Env(env)
        for (g <- a1.asInstanceOf[MalList].value.grouped(2)) {
          let_env.set(g(0).asInstanceOf[Symbol],EVAL(g(1),let_env))
        }
        return EVAL(a2, let_env)
      }
      case Symbol("do") :: rest => {
        val el = rest.map(EVAL(_, env))
        return el.last
      }
      case Symbol("if") :: a1 :: a2 :: rest => {
        val cond = EVAL(a1, env)
        if (cond == null || cond == false) {
          if (rest.length == 0) return null
          return EVAL(rest(0), env)
        } else {
          return EVAL(a2, env)
        }
      }
      case Symbol("fn*") :: a1 :: a2 :: Nil => {
        return new Func((args: List[Any]) => {
          EVAL(a2, new Env(env, types._toIter(a1), args.iterator))
        })
      }
      case _ => {
        // function call
        ast.asInstanceOf[MalList].map(EVAL(_, env)).value match {
          case f :: el => {
            var fn: Func = null
            try {
              fn = f.asInstanceOf[Func]
            } catch {
              case _: Throwable =>
                throw new Exception("attempt to call non-function")
            }
            return fn(el)
          }
          case _ => throw new Exception("invalid apply")
        }
      }
    }
  }

  // print
  def PRINT(exp: Any): String = {
    printer._pr_str(exp, true)
  }

  // repl
  def main(args: Array[String]) = {
    val repl_env: Env = new Env()
    val REP = (str: String) => PRINT(EVAL(READ(str), repl_env))

    // core.scala: defined using scala
    core.ns.map{case (k: String,v: Any) => {
      repl_env.set(Symbol(k), new Func(v))
    }}

    // core.mal: defined using the language itself
    REP("(def! not (fn* (a) (if a false true)))")

    var line:String = null
    while ({line = readLine("user> "); line != null}) {
      try {
        println(REP(line))
      } catch {
        case e : Throwable => {
          println("Error: " + e.getMessage)
          println("    " + e.getStackTrace.mkString("\n    "))
        }
      }
    }
  }
}

// vim: ts=2:sw=2
