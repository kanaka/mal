import types.Function

object printer {
  def _pr_str(obj: Any, print_readably: Boolean = true): String = {
    val _r = print_readably
    return obj match {
      case l: List[Any]   => "(" + l.map(_pr_str(_, _r)).mkString(" ") + ")"
      case v: Array[Any]  => "[" + v.map(_pr_str(_, _r)).mkString(" ") + "]"
      case m: Map[String @unchecked,Any @unchecked] => {
        val lst = m.map{case (k,v) => List(k, v)}.flatten
        "{" + lst.map(_pr_str(_,_r)).mkString(" ") + "}"
      }
      case s: String      => {
        if (s.length > 0 && s(0) == '\u029e') {
          ":" + s.substring(1,s.length)
        } else if (_r) {
          //println("here1: " + s)
          "\"" + s.replace("\\", "\\\\")
                  .replace("\"", "\\\"")
                  .replace("\n", "\\n") + "\""
        } else {
          s
        }
      }
      case Symbol(s)      => s
      case a: types.Atom  => "(atom " + a.value + ")"
      case null           => "nil"
      case _              => {
        if (obj.isInstanceOf[Function]) {
          val f = obj.asInstanceOf[Function]
          "<function (fn* " + _pr_str(f.params) + " " + _pr_str(f.ast) + ")>"
        } else {
          obj.toString
        }
      }
    }
  }

  def _pr_list(lst: List[Any], print_readably: Boolean = true,
               sep: String = " "): String = {
    lst.map{_pr_str(_, print_readably)}.mkString(sep)
  }
}

// vim: ts=2:sw=2
