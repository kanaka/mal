object step0_repl {
  def READ(str: String): String = {
    str
  }

  def EVAL(str: String, env: String): String = {
    str
  }

  def PRINT(str: String): String = {
    str
  }

  def REP(str: String): String = {
    PRINT(EVAL(READ(str), ""))
  }

  def main(args: Array[String]) {
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
