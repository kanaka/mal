import scala.collection.mutable
import scala.io.Source

import printer._pr_list

object core {
  def mal_throw(a: List[Any]) = {
    throw new types.MalException(printer._pr_str(a(0))).init(a(0))
  }

  // Scalar functions
  def keyword(a: List[Any]) = {
    "\u029e" + a(0).asInstanceOf[String]
  }

  def keyword_Q(a: List[Any]) = {
    a(0) match {
      case s: String => s(0) == '\u029e'
      case _ => false
    }
  }

  // string functions
  def read_string(a: List[Any]) = {
    reader.read_str(a(0).asInstanceOf[String])
  }

  def slurp(a: List[Any]) = {
    Source.fromFile(a(0).asInstanceOf[String]).getLines.mkString("\n")
  }

  // Hash Map functions
  def assoc(a: List[Any]): Any = {
    a(0).asInstanceOf[Map[String,Any]] ++
      (types._hash_map(a.drop(1)).asInstanceOf[Map[String,Any]])
  }

  def dissoc(a: List[Any]): Any = {
    var kSet = types._toList(a.drop(1)).toSet
    a(0).asInstanceOf[Map[String,Any]]
      .filterKeys{ !kSet.contains(_) }
  }

  def get(a: List[Any]): Any = {
    val hm = a(0).asInstanceOf[Map[String,Any]]
    val key = a(1).asInstanceOf[String]
    if (hm != null && hm.contains(key)) hm(key) else null
  }

  def contains_Q(a: List[Any]): Any = {
    a(0).asInstanceOf[Map[String,Any]]
      .contains(a(1).asInstanceOf[String])
  }


  // sequence functions
  def concat(a: List[Any]): List[Any] = {
    (for (sq <- a) yield types._toIter(sq)).flatten
  }

  def nth(a: List[Any]): Any = {
    val lst = types._toList(a(0))
    val idx = a(1).asInstanceOf[Int]
    if (idx < lst.length) {
      lst(idx)
    } else {
      throw new Exception("nth: index out of range")
    }
  }

  def first(a: List[Any]): Any = {
    val lst = types._toList(a(0))
    if (lst.length > 0) lst(0) else null
  }


  def apply(a: List[Any]): Any = {
    a match {
      case f :: rest => {
        var args1 = rest.slice(0,rest.length-1)
        var args = args1 ++ types._toList(rest(rest.length-1))
        types._apply(f, args)
      }
      case _ => throw new Exception("invalid apply call")
    }
  }

  def do_map(a: List[Any]): Any = {
    a match {
      case f :: seq :: Nil => {
        types._toList(seq).map(x => types._apply(f,List(x)))
      }
      case _ => throw new Exception("invalid map call")
    }
  }


  // atom functions
  def reset_BANG(a: List[Any]): Any = {
    a(0).asInstanceOf[types.Atom].value = a(1)
    a(1)
  }

  def swap_BANG(a: List[Any]): Any = {
    a match {
      case a0 :: f :: rest => {
        val atm = a0.asInstanceOf[types.Atom]
        val args = atm.value +: rest
        atm.value = types._apply(f, args)
        atm.value
      }
      case _ => throw new Exception("invalid swap! call")
    }
  }


  val ns: Map[String, Any] = Map(
    "="  -> ((a: List[Any]) => types._equal_Q(a(0), a(1))),
    "throw" -> mal_throw _,
    "nil?" -> ((a: List[Any]) => a(0) == null),
    "true?" -> ((a: List[Any]) => a(0) == true),
    "false?" -> ((a: List[Any]) => a(0) == false),
    "symbol" -> ((a: List[Any]) => Symbol(a(0).asInstanceOf[String])),
    "symbol?" -> ((a: List[Any]) => a(0).isInstanceOf[Symbol]),
    "keyword" -> keyword _,
    "keyword?" -> keyword_Q _,

    "pr-str" -> ((a: List[Any]) => _pr_list(a, true, " ")),
    "str" -> ((a: List[Any]) => _pr_list(a, false, "")),
    "prn" -> ((a: List[Any]) => { println(_pr_list(a, true, " ")); null}),
    "println" -> ((a: List[Any]) => { println(_pr_list(a, false, " ")); null}),
    "read-string" -> read_string _,
    "slurp" -> slurp _,
    "<"  -> ((a: List[Any]) => a(0).asInstanceOf[Int] <  a(1).asInstanceOf[Int]),
    "<=" -> ((a: List[Any]) => a(0).asInstanceOf[Int] <= a(1).asInstanceOf[Int]),
    ">"  -> ((a: List[Any]) => a(0).asInstanceOf[Int] >  a(1).asInstanceOf[Int]),
    ">=" -> ((a: List[Any]) => a(0).asInstanceOf[Int] >= a(1).asInstanceOf[Int]),
    "+"  -> ((a: List[Any]) => a(0).asInstanceOf[Int] + a(1).asInstanceOf[Int]),
    "-"  -> ((a: List[Any]) => a(0).asInstanceOf[Int] - a(1).asInstanceOf[Int]),
    "*"  -> ((a: List[Any]) => a(0).asInstanceOf[Int] * a(1).asInstanceOf[Int]),
    "/"  -> ((a: List[Any]) => a(0).asInstanceOf[Int] / a(1).asInstanceOf[Int]),

    "list" -> ((a: List[Any]) => a),
    "list?" -> ((a: List[Any]) => a(0).isInstanceOf[List[Any]]),
    "vector" -> ((a: List[Any]) => a.toArray),
    "vector?" -> ((a: List[Any]) => a(0).isInstanceOf[Array[Any]]),
    "hash-map" -> ((a: List[Any]) => types._hash_map(a)),
    "map?" -> ((a: List[Any]) => a(0).isInstanceOf[Map[String,Any] @unchecked]),
    "assoc" -> assoc _,
    "dissoc" -> dissoc _,
    "get" -> get _,
    "contains?" -> contains_Q _,
    "keys" -> ((a: List[Any]) => a(0).asInstanceOf[Map[String,Any]].keys.toList),
    "vals" -> ((a: List[Any]) => a(0).asInstanceOf[Map[String,Any]].values.toList),

    "sequential?" -> ((a: List[Any]) => types._sequential_Q(a(0))),
    "cons" -> ((a: List[Any]) => a(0) +: types._toList(a(1))),
    "concat" -> concat _,
    "nth" -> nth _,
    "first" -> first _,
    "rest" -> ((a: List[Any]) => types._toList(a(0)).drop(1)),
    "empty?" -> ((a: List[Any]) => types._toIter(a(0)).isEmpty),
    "count" -> ((a: List[Any]) => types._toIter(a(0)).length),
    "conj" -> ((a: List[Any]) => null),
    "apply" -> apply _,
    "map" -> do_map _,

    "with-meta" -> ((a: List[Any]) => null),
    "meta" -> ((a: List[Any]) => null),
    "atom" -> ((a: List[Any]) => new types.Atom(a(0))),
    "atom?" -> ((a: List[Any]) => a(0).isInstanceOf[types.Atom]),
    "deref" -> ((a: List[Any]) => a(0).asInstanceOf[types.Atom].value),
    "reset!" -> reset_BANG _,
    "swap!" -> swap_BANG _
    )
}

// vim:ts=2:sw=2
