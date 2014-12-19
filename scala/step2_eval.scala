import reader.tokenize

object step2_eval {
  // read
  def READ(str: String): Any = {
    reader.read_str(str)
  }

  // eval
  def eval_ast(ast: Any, env: Map[Symbol,Any]): Any = {
    ast match {
      case s : Symbol    => env(s)
      case l: List[Any]  => l.map(EVAL(_, env))
      case v: Array[Any] => v.map(EVAL(_, env)).toArray
      case m: Map[String @unchecked,Any @unchecked] => {
        m.map{case (k: String,v: Any) => (k, EVAL(v, env))}.toMap
      }
      case _             => ast
    }
  }

  def EVAL(ast: Any, env: Map[Symbol,Any]): Any = {
    //println("EVAL: " + printer._pr_str(ast,true))
    if (!ast.isInstanceOf[List[Any]])
      return eval_ast(ast, env)

    // apply list
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

  // print
  def PRINT(exp: Any): String = {
    printer._pr_str(exp, true)
  }

  // repl
  def main(args: Array[String]) = {
    val repl_env: Map[Symbol,Any] = Map(
      '+ -> ((a: List[Any]) => a(0).asInstanceOf[Int] + a(1).asInstanceOf[Int]),
      '- -> ((a: List[Any]) => a(0).asInstanceOf[Int] - a(1).asInstanceOf[Int]),
      '* -> ((a: List[Any]) => a(0).asInstanceOf[Int] * a(1).asInstanceOf[Int]),
      '/ -> ((a: List[Any]) => a(0).asInstanceOf[Int] / a(1).asInstanceOf[Int]))
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
