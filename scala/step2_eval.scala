import types.{MalList, _list_Q, MalVector, MalHashMap, MalFunction}

object step2_eval {
  // read
  def READ(str: String): Any = {
    reader.read_str(str)
  }

  // eval
  def eval_ast(ast: Any, env: Map[Symbol,Any]): Any = {
    ast match {
      case s : Symbol    => env(s)
      case v: MalVector  => v.map(EVAL(_, env))
      case l: MalList    => l.map(EVAL(_, env))
      case m: MalHashMap => {
        m.map{case (k: String,v: Any) => (k, EVAL(v, env))}
      }
      case _             => ast
    }
  }

  def EVAL(ast: Any, env: Map[Symbol,Any]): Any = {
    //println("EVAL: " + printer._pr_str(ast,true))
    if (!_list_Q(ast))
      return eval_ast(ast, env)

    // apply list
    eval_ast(ast, env).asInstanceOf[MalList].value match {
      case f :: el => {
        var fn: List[Any] => Any = null
        try {
          fn = f.asInstanceOf[List[Any] => Any]
        } catch {
          case _: Throwable =>
            throw new Exception("attempt to call non-function")
        }
        return fn(el)
      }
      case _ => throw new Exception("invalid apply")
    }
  }

  // print
  def PRINT(exp: Any): String = {
    printer._pr_str(exp, true)
  }

  // repl
  def main(args: Array[String]) = {
    val repl_env: Map[Symbol,Any] = Map(
      '+ -> ((a: List[Any]) => a(0).asInstanceOf[Long] + a(1).asInstanceOf[Long]),
      '- -> ((a: List[Any]) => a(0).asInstanceOf[Long] - a(1).asInstanceOf[Long]),
      '* -> ((a: List[Any]) => a(0).asInstanceOf[Long] * a(1).asInstanceOf[Long]),
      '/ -> ((a: List[Any]) => a(0).asInstanceOf[Long] / a(1).asInstanceOf[Long]))
    val REP = (str: String) => {
      PRINT(EVAL(READ(str), repl_env))
    }

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
