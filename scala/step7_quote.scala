import types.{MalList, _list, _list_Q, MalVector, MalHashMap,
              Func, MalFunction}
import env.Env

object step7_quote {
  // read
  def READ(str: String): Any = {
    reader.read_str(str)
  }

  // eval
  def is_pair(x: Any): Boolean = {
    types._sequential_Q(x) && x.asInstanceOf[MalList].value.length > 0
  }

  def quasiquote(ast: Any): Any = {
    if (!is_pair(ast)) {
      return _list(Symbol("quote"), ast)
    } else {
      val a0 = ast.asInstanceOf[MalList](0)
      if (types._symbol_Q(a0) &&
          a0.asInstanceOf[Symbol].name == "unquote") {
        return ast.asInstanceOf[MalList](1)
      } else if (is_pair(a0)) {
        val a00 = a0.asInstanceOf[MalList](0)
        if (types._symbol_Q(a00) &&
            a00.asInstanceOf[Symbol].name == "splice-unquote") {
          return _list(Symbol("concat"),
                       a0.asInstanceOf[MalList](1),
                       quasiquote(ast.asInstanceOf[MalList].drop(1)))
        }
      }
      return _list(Symbol("cons"),
                   quasiquote(a0),
                   quasiquote(ast.asInstanceOf[MalList].drop(1)))
    }
  }

  def eval_ast(ast: Any, env: Env): Any = {
    ast match {
      case s : Symbol    => env.get(s)
      case v: MalVector  => v.map(EVAL(_, env))
      case l: MalList    => l.map(EVAL(_, env))
      case m: MalHashMap => {
        m.map{case (k: String,v: Any) => (k, EVAL(v, env))}
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
    REP("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))")

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
