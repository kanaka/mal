import scala.collection.mutable
import scala.io.Source

import types.{MalList, _list, _list_Q,
              MalVector, _vector, _vector_Q,
              MalHashMap, _hash_map_Q, _hash_map,
              Func, MalFunction}
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
      case s: String => s.length != 0 && s(0) == '\u029e'
      case _ => false
    }
  }

  def string_Q(a: List[Any]) = {
    a(0) match {
      case s: String => s.length == 0 || s(0) != '\u029e'
      case _ => false
    }
  }

  // number functions
  def _bool_op(a: List[Any], op: (Long, Long) => Boolean) = {
    op(a(0).asInstanceOf[Long],a(1).asInstanceOf[Long])
  }

  def _num_op(a: List[Any], op: (Long, Long) => Long) = {
    op(a(0).asInstanceOf[Long],a(1).asInstanceOf[Long])
  }


  // string functions
  def read_string(a: List[Any]) = {
    reader.read_str(a(0).asInstanceOf[String])
  }

  def slurp(a: List[Any]) = {
    Source.fromFile(a(0).asInstanceOf[String]).getLines.mkString("\n") + "\n"
  }

  // Hash Map functions
  def assoc(a: List[Any]): Any = {
    a(0).asInstanceOf[MalHashMap] ++ _hash_map(a.drop(1):_*)
  }

  def dissoc(a: List[Any]): Any = {
    var kSet = a.drop(1).toSet
    a(0).asInstanceOf[MalHashMap]
      .filterKeys{ !kSet.contains(_) }
  }

  def get(a: List[Any]): Any = {
    val hm = a(0).asInstanceOf[MalHashMap]
    val key = a(1).asInstanceOf[String]
    if (hm != null && hm.value.contains(key)) hm(key) else null
  }

  def contains_Q(a: List[Any]): Any = {
    a(0).asInstanceOf[MalHashMap].value
      .contains(a(1).asInstanceOf[String])
  }


  // sequence functions
  def concat(a: List[Any]): Any = {
    _list((for (sq <- a) yield types._toIter(sq)).flatten:_*)
  }

  def nth(a: List[Any]): Any = {
    val lst = a(0).asInstanceOf[MalList].value
    val idx = a(1).asInstanceOf[Long]
    if (idx < lst.length) {
      lst(idx.toInt)
    } else {
      throw new Exception("nth: index out of range")
    }
  }

  def first(a: List[Any]): Any = {
    a(0) match {
      case null => null
      case ml: MalList => {
        val lst = ml.value
        if (lst.length > 0) lst(0) else null
      }
    }
  }

  def rest(a: List[Any]): Any = {
    a(0) match {
      case null => _list()
      case ml: MalList => _list(ml.drop(1).value:_*)
    }
  }

  def empty_Q(a: List[Any]): Any = {
    a(0) match {
      case null => true
      case ml: MalList => ml.value.isEmpty
    }
  }

  def count(a: List[Any]): Any = {
    a(0) match {
      case null => 0
      case ml: MalList => ml.value.length.asInstanceOf[Long]
    }
  }

  def apply(a: List[Any]): Any = {
    a match {
      case f :: rest => {
        var args1 = rest.slice(0,rest.length-1)
        var args = args1 ++ rest(rest.length-1).asInstanceOf[MalList].value
        types._apply(f, args)
      }
      case _ => throw new Exception("invalid apply call")
    }
  }

  def do_map(a: List[Any]): Any = {
    a match {
      case f :: seq :: Nil => {
        var res = seq.asInstanceOf[MalList].map(x => types._apply(f,List(x)));
        _list(res.value:_*)
      }
      case _ => throw new Exception("invalid map call")
    }
  }

  def conj(a: List[Any]): Any = {
    a(0) match {
      case mv: MalVector => {
        _vector(mv.value ++ a.slice(1,a.length):_*)
      }
      case ml: MalList => {
        _list(a.slice(1,a.length).reverse ++ ml.value:_*)
      }
    }
  }

  def seq(a: List[Any]): Any = {
    a(0) match {
      case mv: MalVector => {
        if (mv.value.length == 0) null else _list(mv.value:_*)
      }
      case ml: MalList => {
        if (ml.value.length == 0) null else ml
      }
      case ms: String => {
        if (ms.length == 0) null else _list(ms.split("(?!^)"):_*)
      }
      case null => null
      case _ => throw new Exception("seq: called on non-sequence")
    }
  }


  // meta functions
  def with_meta(a: List[Any]): Any = {
    val meta: Any = a(1)
    a(0) match {
      case ml: MalList => {
        val new_ml = ml.clone()
        new_ml.meta = meta
        new_ml
      }
      case hm: MalHashMap => {
        val new_hm = hm.clone()
        new_hm.meta = meta
        new_hm
      }
      case fn: Func => {
        val new_fn = fn.clone()
        new_fn.meta = meta
        new_fn
      }
      case fn: MalFunction => {
        val new_fn = fn.clone()
        new_fn.meta = meta
        new_fn
      }
      case _ => throw new Exception("no meta support for " + a(0).getClass)
    }
  }

  def meta(a: List[Any]): Any = {
    a(0) match {
      case ml: MalList     => ml.meta
      case hm: MalHashMap  => hm.meta
      case fn: Func        => fn.meta
      case fn: MalFunction => fn.meta
      case _ => throw new Exception("no meta support for " + a(0).getClass)
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


  val ns: Map[String, (List[Any]) => Any] = Map(
    "="  -> ((a: List[Any]) => types._equal_Q(a(0), a(1))),
    "throw" -> mal_throw _,
    "nil?" -> ((a: List[Any]) => a(0) == null),
    "true?" -> ((a: List[Any]) => a(0) == true),
    "false?" -> ((a: List[Any]) => a(0) == false),
    "string?" -> string_Q _,
    "symbol" -> ((a: List[Any]) => Symbol(a(0).asInstanceOf[String])),
    "symbol?" -> ((a: List[Any]) => a(0).isInstanceOf[Symbol]),
    "keyword" -> keyword _,
    "keyword?" -> keyword_Q _,

    "pr-str" -> ((a: List[Any]) => _pr_list(a, true, " ")),
    "str" -> ((a: List[Any]) => _pr_list(a, false, "")),
    "prn" -> ((a: List[Any]) => { println(_pr_list(a, true, " ")); null}),
    "println" -> ((a: List[Any]) => { println(_pr_list(a, false, " ")); null}),
    "readline" -> ((a: List[Any]) => readLine(a(0).asInstanceOf[String])),
    "read-string" -> read_string _,
    "slurp" -> slurp _,

    "<"  -> ((a: List[Any]) => _bool_op(a, _ < _)),
    "<=" -> ((a: List[Any]) => _bool_op(a, _ <= _)),
    ">"  -> ((a: List[Any]) => _bool_op(a, _ > _)),
    ">=" -> ((a: List[Any]) => _bool_op(a, _ >= _)),
    "+"  -> ((a: List[Any]) => _num_op(a, _ + _)),
    "-"  -> ((a: List[Any]) => _num_op(a, _ - _)),
    "*"  -> ((a: List[Any]) => _num_op(a, _ * _)),
    "/"  -> ((a: List[Any]) => _num_op(a, _ / _)),
    "time-ms" -> ((a: List[Any]) => System.currentTimeMillis),

    "list" -> ((a: List[Any]) => _list(a:_*)),
    "list?" -> ((a: List[Any]) => _list_Q(a(0))),
    "vector" -> ((a: List[Any]) => _vector(a:_*)),
    "vector?" -> ((a: List[Any]) => _vector_Q(a(0))),
    "hash-map" -> ((a: List[Any]) => _hash_map(a:_*)),
    "map?" -> ((a: List[Any]) => _hash_map_Q(a(0))),
    "assoc" -> assoc _,
    "dissoc" -> dissoc _,
    "get" -> get _,
    "contains?" -> contains_Q _,
    "keys" -> ((a: List[Any]) => a(0).asInstanceOf[MalHashMap].keys),
    "vals" -> ((a: List[Any]) => a(0).asInstanceOf[MalHashMap].vals),

    "sequential?" -> ((a: List[Any]) => types._sequential_Q(a(0))),
    "cons" -> ((a: List[Any]) => a(0) +: a(1).asInstanceOf[MalList]),
    "concat" -> concat _,
    "nth" -> nth _,
    "first" -> first _,
    "rest" -> rest _,
    "empty?" -> empty_Q _,
    "count" -> count _,
    "apply" -> apply _,
    "map" -> do_map _,

    "conj" -> conj _,
    "seq" -> seq _,

    "with-meta" -> with_meta _,
    "meta" -> meta _,
    "atom" -> ((a: List[Any]) => new types.Atom(a(0))),
    "atom?" -> ((a: List[Any]) => a(0).isInstanceOf[types.Atom]),
    "deref" -> ((a: List[Any]) => a(0).asInstanceOf[types.Atom].value),
    "reset!" -> reset_BANG _,
    "swap!" -> swap_BANG _
    )
}

// vim:ts=2:sw=2
