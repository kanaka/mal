import scala.util.matching.Regex

import types.{MalList, _list, MalVector, _vector, MalHashMap, _hash_map}

object reader {

  class Reader (tokens: Array[String]) {
    var data = tokens
    var position: Int = 0
    def peek(): String = {
      if (position >= data.length) return(null)
      data(position)
    }
    def next(): String = {
      if (position >= data.length) return(null)
      position = position + 1
      data(position-1)
    }
  }

  def tokenize(str: String): Array[String] = {
    val re = """[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)""".r
    re.findAllMatchIn(str).map{ _.group(1) }
                          .filter{ s => s != "" && s(0) != ';' }
                          .toArray
  }

  def parse_str(s: String): String = {
    s.replace("\\\"", "\"").replace("\\n", "\n").replace("\\\\", "\\")
  }

  def read_atom(rdr: Reader): Any = {
    val token = rdr.next()
    val re_int = """^(-?[0-9]+)$""".r
    val re_flt = """^(-?[0-9][0-9.]*)$""".r
    val re_str =  """^"(.*)"$""".r
    val re_key = """^:(.*)$""".r
    return token match {
      case re_int(i) => i.toLong      // integer
      case re_flt(f) => f.toDouble    // float
      case re_str(s) => parse_str(s)  // string
      case re_key(k) => "\u029e" + k  // keyword
      case "nil"     => null
      case "true"    => true
      case "false"   => false
      case _         => Symbol(token) // symbol
    }
  }

  def read_list(rdr: Reader,
                start: String = "(", end: String = ")"): MalList = {
    var ast: MalList = _list()
    var token = rdr.next()
    if (token != start) throw new Exception("expected '" + start + "', got EOF")
    while ({token = rdr.peek(); token != end}) {
      if (token == null) throw new Exception("expected '" + end + "', got EOF")
      ast = ast :+ read_form(rdr)
    }
    rdr.next()
    ast
  }

  def read_form(rdr: Reader): Any = {
    return rdr.peek() match {
      case "'"  => { rdr.next; _list(Symbol("quote"), read_form(rdr)) }
      case "`"  => { rdr.next; _list(Symbol("quasiquote"), read_form(rdr)) }
      case "~"  => { rdr.next; _list(Symbol("unquote"), read_form(rdr)) }
      case "~@" => { rdr.next; _list(Symbol("splice-unquote"), read_form(rdr)) }
      case "^"  => { rdr.next; val meta = read_form(rdr);
                     _list(Symbol("with-meta"), read_form(rdr), meta) }
      case "@"  => { rdr.next; _list(Symbol("deref"), read_form(rdr)) }

      case "("  => read_list(rdr)
      case ")"  => throw new Exception("unexpected ')')")
      case "["  => _vector(read_list(rdr, "[", "]").value:_*)
      case "]"  => throw new Exception("unexpected ']')")
      case "{"  => _hash_map(read_list(rdr, "{", "}").value:_*)
      case "}"  => throw new Exception("unexpected '}')")
      case _    => read_atom(rdr)
    }
  }

  def read_str(str: String): Any = {
    val tokens = tokenize(str)
    if (tokens.length == 0) return null
    return read_form(new Reader(tokens))
  }
}

// vim: ts=2:sw=2
