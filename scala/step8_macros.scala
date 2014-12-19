import env.Env
import types.Function

object step8_macros {
  // read
  def READ(str: String): Any = {
    reader.read_str(str)
  }

  // eval
  def is_pair(x: Any): Boolean = {
    types._sequential_Q(x) && types._toIter(x).length > 0
  }

  def quasiquote(ast: Any): Any = {
    if (!is_pair(ast)) {
      return List(Symbol("quote"), ast)
    } else {
      val a0 = types._toList(ast)(0)
      if (types._symbol_Q(a0) &&
          a0.asInstanceOf[Symbol].name == "unquote") {
        return types._toList(ast)(1)
      } else if (is_pair(a0)) {
        val a00 = types._toList(a0)(0)
        if (types._symbol_Q(a00) &&
            a00.asInstanceOf[Symbol].name == "splice-unquote") {
          return List(Symbol("concat"),
                      types._toList(a0)(1),
                      quasiquote(types._toList(ast).drop(1)))
        }
      }
      return List(Symbol("cons"),
                  quasiquote(a0),
                  quasiquote(types._toList(ast).drop(1)))
    }
  }

  def is_macro_call(ast: Any, env: Env): Boolean = {
    ast match {
      case l: List[Any] => {
        if (types._symbol_Q(l(0)) &&
            env.find(l(0).asInstanceOf[Symbol]) != null) {
          env.get(l(0).asInstanceOf[Symbol]) match {
            case f: Function => return f.ismacro
            case _ => return false
          }
        }
        return false
      }
      case _ => return false
    }
  }

  def macroexpand(orig_ast: Any, env: Env): Any = {
    var ast = orig_ast;
    while (is_macro_call(ast, env)) {
      ast.asInstanceOf[List[Any]] match {
        case f :: args => {
          val mac = env.get(f.asInstanceOf[Symbol])
          ast = mac.asInstanceOf[Function](args)
        }
        case _ => throw new Exception("macroexpand: invalid call")
      }
    }
    ast
  }

  def eval_ast(ast: Any, env: Env): Any = {
    ast match {
      case s : Symbol    => env.get(s)
      case l: List[Any]  => l.map(EVAL(_, env))
      case v: Array[Any] => v.map(EVAL(_, env)).toArray
      case m: Map[String @unchecked,Any @unchecked] => {
        m.map{case (k: String,v: Any) => (k, EVAL(v, env))}.toMap
      }
      case _             => ast
    }
  }

  def EVAL(orig_ast: Any, orig_env: Env): Any = {
   var ast = orig_ast; var env = orig_env;
   while (true) {

    //println("EVAL: " + printer._pr_str(ast,true))
    if (!ast.isInstanceOf[List[Any]])
      return eval_ast(ast, env)

    // apply list
    ast = macroexpand(ast, env)
    if (!ast.isInstanceOf[List[Any]]) return ast

    ast.asInstanceOf[List[Any]] match {
      case Symbol("def!") :: a1 :: a2 :: Nil => {
        return env.set(a1.asInstanceOf[Symbol], EVAL(a2, env))
      }
      case Symbol("let*") :: a1 :: a2 :: Nil => {
        val let_env = new Env(env)
        for (g <- types._toIter(a1).grouped(2)) {
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
      case Symbol("defmacro!") :: a1 :: a2 :: Nil => {
        val f = EVAL(a2, env)
        f.asInstanceOf[Function].ismacro = true
        return env.set(a1.asInstanceOf[Symbol], f)
      }
      case Symbol("macroexpand") :: a1 :: Nil => {
        return macroexpand(a1, env)
      }
      case Symbol("do") :: rest => {
        eval_ast(rest.slice(0,rest.length-1), env)
        ast = ast.asInstanceOf[List[Any]].last  // continue loop (TCO)
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
        return new Function(a2, env, types._toList(a1),
          (args: List[Any]) => {
            EVAL(a2, new Env(env, types._toIter(a1), args.iterator))
          }
        )
      }
      case _ => {
        // function call
        eval_ast(ast, env) match {
          case f :: el => {
            f match {
              case fn: Function => {
                env = fn.gen_env(el) 
                ast = fn.ast  // continue loop (TCO)
              }
              case fn: ((List[Any]) => Any) @unchecked => {
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
    core.ns.map{case (k: String,v: Any) => { repl_env.set(Symbol(k), v) }}
    repl_env.set(Symbol("eval"), (a: List[Any]) => EVAL(a(0), repl_env))
    repl_env.set(Symbol("*ARGV*"), args.slice(1,args.length).toList)

    // core.mal: defined using the language itself
    REP("(def! not (fn* (a) (if a false true)))")
    REP("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))")
    REP("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")
    REP("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))")


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
