import env.Env

object step5_tco {
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

  def EVAL(orig_ast: Any, orig_env: Env): Any = {
   var ast = orig_ast; var env = orig_env;
   while (true) {

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
        env = let_env
        ast = a2   // continue loop (TCO)
      }
      case Symbol("do") :: rest => {
        eval_ast(rest.slice(1,rest.length-1), env)
        ast = ast.asInstanceOf[List[Any]].last  // continue loop (TCO)
      }
      case Symbol("if") :: a1 :: a2 :: rest => {
        val cond = EVAL(a1, env)
        if (cond == null || cond == false) {
          if (rest.length == 0) return null
          ast = rest(0)  // continue loop (TCO)
        } else {
          ast = a2  // continue loop (TCO)
        }
      }
      case Symbol("fn*") :: a1 :: a2 :: Nil => {
        return new types.Function(a2, env, a1.asInstanceOf[List[Any]],
          (args: List[Any]) => {
            EVAL(a2, new Env(env, types._toIter(a1), args.iterator))
          }
        )
      }
      case _ => {
        // function call
        eval_ast(ast, env) match {
          case f :: el => {
            f match {
              case fn: types.Function => {
                env = fn.gen_env(el) 
                ast = fn.ast  // continue loop (TCO)
              }
              case fn: ((List[Any]) => Any) @unchecked => {
                return fn(el)
              }
              case _ => {
                throw new Exception("attempt to call non-function")
              }
            }
          }
          case _ => throw new Exception("invalid apply")
        }
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
        case e : Throwable => {
          println("Error: " + e.getMessage)
          println("    " + e.getStackTrace.mkString("\n    "))
        }
      }
    }
  }
}

// vim: ts=2:sw=2
