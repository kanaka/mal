import types.{MalList, _list, _list_Q, MalVector, MalHashMap,
              Func, MalFunction}
import env.Env

object step7_quote {
  // read
  def READ(str: String): Any = {
    reader.read_str(str)
  }

  // eval
  def quasiquote_loop(elts: List[Any]): MalList = {
    var acc = _list()
    for (elt <- elts.reverse) {
      if (types._list_Q(elt)) {
        elt.asInstanceOf[MalList].value match {
          case Symbol("splice-unquote") :: x :: Nil => {
            acc = _list(Symbol("concat"), x, acc)
          }
          case _ => {
            acc = _list(Symbol("cons"), quasiquote(elt), acc)
          }
        }
      } else {
        acc = _list(Symbol("cons"), quasiquote(elt), acc)
      }
    }
    return acc
  }

  def quasiquote(ast: Any): Any = {
    ast match {
      //  Test vectors before they match MalList.
      case v: MalVector => {
        _list(Symbol("vec"), quasiquote_loop(v.value))
      }
      case l: MalList => {
        l.value match {
          case Symbol("unquote") :: x :: Nil => x
          case _ => quasiquote_loop(l.value)
        }
      }
      case _ : Symbol     => _list(Symbol("quote"), ast)
      case _ : MalHashMap => _list(Symbol("quote"), ast)
      case _ => ast
    }
  }

  def eval_ast(ast: Any, env: Env): Any = {
    ast match {
      case s : Symbol    => env.get(s)
      case v: MalVector  => v.map(EVAL(_, env))
      case l: MalList    => l.map(EVAL(_, env))
      case m: MalHashMap => {
        m.map{case (k,v) => (k, EVAL(v, env))}
      }
      case _             => ast
    }
  }

  def EVAL(orig_ast: Any, orig_env: Env): Any = {
   var ast = orig_ast; var env = orig_env;
   while (true) {

    //println("EVAL: " + printer._pr_str(ast,true))
    if (!_list_Q(ast))
      return eval_ast(ast, env)

    // apply list
    ast.asInstanceOf[MalList].value match {
      case Nil => {
        return ast
      }
      case Symbol("def!") :: a1 :: a2 :: Nil => {
        return env.set(a1.asInstanceOf[Symbol], EVAL(a2, env))
      }
      case Symbol("let*") :: a1 :: a2 :: Nil => {
        val let_env = new Env(env)
        for (g <- a1.asInstanceOf[MalList].value.grouped(2)) {
          let_env.set(g(0).asInstanceOf[Symbol],EVAL(g(1),let_env))
        }
        env = let_env
        ast = a2   // continue loop (TCO)
      }
      case Symbol("quote") :: a1 :: Nil => {
        return a1
      }
      case Symbol("quasiquoteexpand") :: a1 :: Nil => {
        return quasiquote(a1)
      }
      case Symbol("quasiquote") :: a1 :: Nil => {
        ast = quasiquote(a1)  // continue loop (TCO)
      }
      case Symbol("do") :: rest => {
        eval_ast(_list(rest.slice(0,rest.length-1):_*), env)
        ast = ast.asInstanceOf[MalList].value.last  // continue loop (TCO)
      }
      case Symbol("if") :: a1 :: a2 :: rest => {
        val cond = EVAL(a1, env)
        if (cond == null || cond == false) {
          if (rest.length == 0) return null
          ast = rest(0)  // continue loop (TCO)
        } else {
          ast = a2  // continue loop (TCO)
        }
      }
      case Symbol("fn*") :: a1 :: a2 :: Nil => {
        return new MalFunction(a2, env, a1.asInstanceOf[MalList],
          (args: List[Any]) => {
            EVAL(a2, new Env(env, types._toIter(a1), args.iterator))
          }
        )
      }
      case _ => {
        // function call
        eval_ast(ast, env).asInstanceOf[MalList].value match {
          case f :: el => {
            f match {
              case fn: MalFunction => {
                env = fn.gen_env(el) 
                ast = fn.ast  // continue loop (TCO)
              }
              case fn: Func => {
                return fn(el)
              }
              case _ => {
                throw new Exception("attempt to call non-function: " + f)
              }
            }
          }
          case _ => throw new Exception("invalid apply")
        }
      }
    }
   }
  }

  // print
  def PRINT(exp: Any): String = {
    printer._pr_str(exp, true)
  }

  // repl
  def main(args: Array[String]) = {
    val repl_env: Env = new Env()
    val REP = (str: String) => PRINT(EVAL(READ(str), repl_env))

    // core.scala: defined using scala
    core.ns.map{case (k: String,v: Any) => {
      repl_env.set(Symbol(k), new Func(v))
    }}
    repl_env.set(Symbol("eval"), new Func((a: List[Any]) => EVAL(a(0), repl_env)))
    repl_env.set(Symbol("*ARGV*"), _list(args.slice(1,args.length):_*))

    // core.mal: defined using the language itself
    REP("(def! not (fn* (a) (if a false true)))")
    REP("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")

    if (args.length > 0) {
      REP("(load-file \"" + args(0) + "\")")
      System.exit(0)
    }

    // repl loop
    var line:String = null
    while ({line = readLine("user> "); line != null}) {
      try {
        println(REP(line))
      } catch {
        case e : Throwable => {
          println("Error: " + e.getMessage)
          println("    " + e.getStackTrace.mkString("\n    "))
        }
      }
    }
  }
}

// vim: ts=2:sw=2
