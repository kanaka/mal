import scala.collection._
import scala.collection.generic._

import env.Env
import printer._pr_str

object types {
  class MalException(msg: String) extends Throwable(msg) {
    var value: Any = null
    def init(obj: Any) = { value = obj; this }
  }

  def _toIter(obj: Any): Iterator[Any] = {
    obj match {
      case v: MalVector => v.value.iterator
      case l: MalList => l.value.iterator
      case null => Iterator.empty
      case _ => throw new Exception("cannot convert " +
                                    obj.getClass + " to iterator")
    }
  }

  def _equal_Q(a: Any, b: Any): Any = {
    (a, b) match {
      case (a: MalList, b: MalList)   => {
        if (a.value.length != b.value.length) return false
        for ( (x, y) <- (a.value zip b.value) ) {
          if (_equal_Q(x, y) != true) return false
        }
        true
      }
      case (a: MalHashMap, b: MalHashMap) => {
        if (a.value.size != b.value.size) return false
        for ( (k,v) <- a.value ) {
          if (_equal_Q(v,b.value(k)) != true) return false
        }
        true
      }
      case _ => a == b
    }
  }

  def _sequential_Q(a: Any): Boolean = {
    a match {
      case l: MalList => true
      case _ => false
    }
  }

  def _symbol_Q(a: Any) = { a.isInstanceOf[Symbol] }


  // Lists
  class MalList(seq: Any*) {
    var value: List[Any] = seq.toList
    var meta: Any = null

    override def clone(): MalList = {
      val new_ml = new MalList()
      new_ml.value = value
      new_ml.meta = meta
      new_ml
    }

    def apply(idx: Int): Any = value(idx)
    def map(f: Any => Any) = new MalList(value.map(f):_*)
    def drop(cnt: Int) = new MalList(value.drop(cnt):_*)
    def :+(that: Any) = new MalList((value :+ that):_*)
    def +:(that: Any) = new MalList((that +: value):_*)

    override def toString() = {
      "(" + value.map(_pr_str(_, true)).mkString(" ") + ")"
    }
    def toString(print_readably: Boolean) = {
      "(" + value.map(_pr_str(_, print_readably)).mkString(" ") + ")"
    }
  }
  def _list(seq: Any*) = {
    new MalList(seq:_*)
  }
  def _list_Q(obj: Any) = {
    obj.isInstanceOf[MalList] && !obj.isInstanceOf[MalVector]
  }


  // Vectors
  class MalVector(seq: Any*) extends MalList(seq:_*) {
    override def clone() = {
      val new_mv = new MalVector()
      new_mv.value = value
      new_mv.meta = meta
      new_mv
    }

    override def map(f: Any => Any) = new MalVector(value.map(f):_*)
    override def drop(cnt: Int) = new MalVector(value.drop(cnt):_*)

    override def toString() = {
      "[" + value.map(_pr_str(_, true)).mkString(" ") + "]"
    }
    override def toString(print_readably: Boolean) = {
      "[" + value.map(_pr_str(_, print_readably)).mkString(" ") + "]"
    }
  }
  def _vector(seq: Any*) = {
    new MalVector(seq:_*)
  }
  def _vector_Q(obj: Any) = {
    obj.isInstanceOf[MalVector]
  }


  // Hash Maps
  class MalHashMap(seq: Any*) {
    var value: Map[String,Any] = seq.toList.grouped(2).map(
      (kv: List[Any]) => (kv(0).asInstanceOf[String], kv(1))).toMap
    var meta: Any = null

    override def clone(): MalHashMap = {
      val new_hm = new MalHashMap()
      new_hm.value = value
      new_hm.meta = meta
      new_hm
    }

    def keys(): MalList = new MalList(value.keys.toSeq:_*)
    def vals(): MalList = new MalList(value.values.toSeq:_*)

    def apply(key: String): Any = value(key)
    def map(f: ((String, Any)) => (String, Any)) = {
      val res = value.map(f).map{case (k,v) => List(k,v)}
      new MalHashMap(res.flatten.toSeq:_*)
    }
    def filterKeys(f: String => Boolean) = {
      val res = value.filterKeys(f).map{case (k,v) => List(k,v)}
      new MalHashMap(res.flatten.toSeq:_*)
    }
    def ++(that: MalHashMap) = {
      val new_hm = clone() 
      new_hm.value ++= that.value
      new_hm
    }

    override def toString() = {
      var res = mutable.MutableList[Any]()
      for ((k,v) <- value) {
        res += _pr_str(k, true)
        res += _pr_str(v, true)
      }
      "{" + res.mkString(" ") + "}"
    }
    def toString(print_readably: Boolean) = {
      var res = mutable.MutableList[Any]()
      for ((k,v) <- value) {
        res += _pr_str(k, print_readably)
        res += _pr_str(v, print_readably)
      }
      "{" + res.mkString(" ") + "}"
    }
  }
  def _hash_map(seq: Any*) = {
    new MalHashMap(seq:_*)
  }
  def _hash_map_Q(obj: Any) = {
    obj.isInstanceOf[MalHashMap]
  }


  // Function types

  class Func(_fn: ((List[Any]) => Any)) {
    val fn = _fn
    var meta: Any = null

    override def clone(): Func = {
      val new_fn = new Func(fn)
      new_fn.meta = meta
      new_fn
    }

    def apply(args: List[Any]): Any = fn(args)
  }

  class MalFunction(_ast: Any, _env: Env, _params: MalList,
                    fn: ((List[Any]) => Any)) {
    val ast = _ast
    val env = _env
    val params = _params
    var ismacro = false
    var meta: Any = null

    override def clone(): MalFunction = {
      val new_fn = new MalFunction(ast, env, params, fn)
      new_fn.ismacro = ismacro
      new_fn.meta = meta
      new_fn
    }

    def apply(args: List[Any]): Any = fn(args)

    def gen_env(args: List[Any]): Env = {
      return new Env(env, params.value.iterator, args.iterator)
    }
  }

  def _apply(f: Any, args: List[Any]): Any = {
    f match {
      case fn: types.MalFunction => fn(args)
      case fn: Func              => fn(args)
      case _ => throw new Exception("attempt to call non-function")
    }
  }

  def _hash_map(lst: List[Any]): Any = {
    lst.grouped(2).map(
      (kv: List[Any]) => (kv(0).asInstanceOf[String], kv(1))).toMap
  }

  class Atom(_value: Any) {
    var value = _value
  }
}

// vim:ts=2:sw=2
