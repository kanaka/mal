import types.{MalList, _list_Q, MalVector, MalHashMap, MalFunction}

object step2_eval {
  // read
  def READ(str: String): Any = {
    reader.read_str(str)
  }

  // eval
  def EVAL(ast: Any, env: Map[Symbol,Any]): Any = {

    //  println("EVAL: " + printer._pr_str(ast,true))

    ast match {
      case s : Symbol    => return env(s)
      case v: MalVector  => return v.map(EVAL(_, env))
      case l: MalList    => {}
      case m: MalHashMap => {
        return m.map{case (k,v) => (k, EVAL(v, env))}
      }
      case _             => return ast
    }

    // apply list
    if (ast.asInstanceOf[MalList].value.length == 0)
      return ast
    ast.asInstanceOf[MalList].map(EVAL(_, env)).value match {
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
