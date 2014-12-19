import scala.collection._
import scala.collection.generic._

import env.Env

object types {
  class MalException(msg: String) extends Throwable(msg) {
    var value: Any = null
    def init(obj: Any) = { value = obj; this }
  }

  def _toIter(obj: Any): Iterator[Any] = {
    obj match {
      case l: List[Any] => l.iterator
      case v: Array[Any] => v.iterator
      case null => Iterator.empty
      case _ => throw new Exception("cannot convert " +
                                    obj.getClass + " to iterator")
    }
  }

  def _toList(obj: Any): List[Any] = {
    obj match {
      case l: List[Any] => l
      case v: Array[Any] => v.toList
      case null => List()
      case _ => throw new Exception("cannot convert " +
                                    obj.getClass + " to list")
    }
  }

  def _equal_Q(a: Any, b: Any): Any = {
    (a, b) match {
      case (a: List[Any], b: List[Any])   => a == b
      case (a: Array[Any], b: Array[Any]) => a.deep == b.deep
      case (a: List[Any], b: Array[Any])  => a == b.deep
      case (a: Array[Any], b: List[Any])  => a.deep == b
      case (a: Map[String @unchecked,Any @unchecked],
            b: Map[String @unchecked,Any @unchecked]) => a == b
      case _ => a == b
    }
  }

  def _sequential_Q(a: Any): Boolean = {
    a match {
      case l: List[Any] => true
      case v: Array[Any] => true
      case _ => false
    }
  }

  def _symbol_Q(a: Any) = { a.isInstanceOf[Symbol] }


  // Lists

  class MalList[A](seq : A*) extends Traversable[A]
                             with GenericTraversableTemplate[A, MalList]
                             with TraversableLike[A, MalList[A]] {
    var meta: Any = null
    override def companion = MalList
    def foreach[U](f: A => U) = seq.foreach(f)
  }
  object MalList extends TraversableFactory[MalList] {
    implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, MalList[A]] = new GenericCanBuildFrom[A]
    def newBuilder[A] = new scala.collection.mutable.LazyBuilder[A,MalList[A]] {
      def result = {
        val data = parts.foldLeft(List[A]()){(l,n) => l ++ n}
        new MalList(data:_*)
      }
    }
  }


  // Vectors
  class MalVector[A](seq : A*) extends Traversable[A]
                               with GenericTraversableTemplate[A, MalVector]
                               with TraversableLike[A, MalVector[A]] {
    var meta: Any = null
    override def companion = MalVector
    def foreach[U](f: A => U) = seq.foreach(f)
  }
  object MalVector extends TraversableFactory[MalVector] {
    implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, MalVector[A]] = new GenericCanBuildFrom[A]
    def newBuilder[A] = new scala.collection.mutable.LazyBuilder[A,MalVector[A]] {
      def result = {
        val data = parts.foldLeft(List[A]()){(l,n) => l ++ n}
        new MalVector(data:_*)
      }
    }
  }


  class Function(_ast: Any, _env: Env, _params: List[Any],
                 fn: ((List[Any]) => Any)) {
    val ast = _ast
    val env = _env
    val params = _params
    var ismacro = false

    def apply(args: List[Any]): Any = {
      fn(args)
    }

    def gen_env(args: List[Any]): Env = {
      return new Env(env, params.iterator, args.iterator)
    }
  }

  def _apply(f: Any, args: List[Any]): Any = {
    f match {
      case fn: types.Function => fn(args)
      case fn: ((List[Any]) => Any) @unchecked => fn(args)
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
