import types.{MalList, _list_Q, MalVector, MalHashMap, MalFunction}
import env.Env

object step3_env {
  // read
  def READ(str: String): Any = {
    reader.read_str(str)
  }

  // eval
  def eval_ast(ast: Any, env: Env): Any = {
    ast match {
      case s : Symbol    => env.get(s)
      case v: MalVector  => v.map(EVAL(_, env))
      case l: MalList    => l.map(EVAL(_, env))
      case m: MalHashMap => {
        m.map{case (k,v) => (k, EVAL(v, env))}
      }
      case _             => ast
    }
  }

  def EVAL(ast: Any, env: Env): Any = {
    //println("EVAL: " + printer._pr_str(ast,true))
    if (!_list_Q(ast))
      return eval_ast(ast, env)

    // apply list
    ast.asInstanceOf[MalList].value match {
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
      case _ => {
        // function call
        eval_ast(ast, env).asInstanceOf[MalList].value match {
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
    repl_env.set('+, (a: List[Any]) => a(0).asInstanceOf[Long] + a(1).asInstanceOf[Long])
    repl_env.set('-, (a: List[Any]) => a(0).asInstanceOf[Long] - a(1).asInstanceOf[Long])
    repl_env.set('*, (a: List[Any]) => a(0).asInstanceOf[Long] * a(1).asInstanceOf[Long])
    repl_env.set('/, (a: List[Any]) => a(0).asInstanceOf[Long] / a(1).asInstanceOf[Long])
    val REP = (str: String) => {
      PRINT(EVAL(READ(str), repl_env))
    }

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
