import reader.tokenize

object step1_read_print {
  // read
  def READ(str: String): Any = {
    reader.read_str(str)
  }

  // eval
  def EVAL(ast: Any, env: String): Any = {
    ast
  }

  // print
  def PRINT(exp: Any): String = {
    printer._pr_str(exp, true)
  }

  // repl
  def main(args: Array[String]) = {
    val REP = (str: String) => {
      PRINT(EVAL(READ(str), ""))
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
