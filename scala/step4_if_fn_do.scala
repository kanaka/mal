import env.Env

object step4_if_fn_do {
  // read
  def READ(str: String): Any = {
    reader.read_str(str)
  }

  // eval
  def eval_ast(ast: Any, env: Env): Any = {
    ast match {
      case s : Symbol    => env.get(s)
      case l: List[Any]  => l.map(EVAL(_, env))
      case v: Array[Any] => v.map(EVAL(_, env)).toArray
      case m: Map[String @unchecked,Any @unchecked] => {
        m.map{case (k: String,v: Any) => (k, EVAL(v, env))}.toMap
      }
      case _             => ast
    }
  }

  def EVAL(ast: Any, env: Env): Any = {
    //println("EVAL: " + printer._pr_str(ast,true))
    if (!ast.isInstanceOf[List[Any]])
      return eval_ast(ast, env)

    // apply list
    ast.asInstanceOf[List[Any]] match {
      case Symbol("def!") :: a1 :: a2 :: Nil => {
        return env.set(a1.asInstanceOf[Symbol], EVAL(a2, env))
      }
      case Symbol("let*") :: a1 :: a2 :: Nil => {
        val let_env = new Env(env)
        for (g <- types._toIter(a1).grouped(2)) {
          let_env.set(g(0).asInstanceOf[Symbol],EVAL(g(1),let_env))
        }
        return EVAL(a2, let_env)
      }
      case Symbol("do") :: rest => {
        val el = eval_ast(rest, env)
        return el.asInstanceOf[List[Any]].last
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
        return (args: List[Any]) => {
          EVAL(a2, new Env(env, types._toIter(a1), args.iterator))
        }
      }
      case _ => {
        // function call
        eval_ast(ast, env) match {
          case f :: el => {
            var fn: List[Any] => Any = null
            try {
              fn = f.asInstanceOf[(List[Any]) => Any]
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
    core.ns.map{case (k: String,v: Any) => { repl_env.set(Symbol(k), v) }}

    // core.mal: defined using the language itself
    REP("(def! not (fn* (a) (if a false true)))")

    var line:String = null
    while ({line = readLine("user> "); line != null}) {
      try {
        println(REP(line))
      } catch {
        case e : Exception => {
          println("Error: " + e.getMessage)
          println("    " + e.getStackTrace.mkString("\n    "))
        }
      }
    }
  }
}

// vim: ts=2:sw=2
