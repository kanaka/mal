import "os" for Process
import "./env" for Env
import "./readline" for Readline
import "./reader" for MalReader
import "./printer" for Printer
import "./types" for MalSymbol, MalSequential, MalList, MalVector, MalMap, MalNativeFn, MalFn, MalException
import "./core" for Core

class Mal {
  static read(str) {
    return MalReader.read_str(str)
  }

  static qq_loop(elt, acc) {
    if (elt is MalList && elt.count == 2 && elt[0] is MalSymbol && elt[0].value == "splice-unquote") {
      return MalList.new([MalSymbol.new("concat"), elt[1], acc])
    } else {
      return MalList.new([MalSymbol.new("cons"), quasiquote(elt), acc])
    }
  }

  static qq_foldr(ast) {
    var acc = MalList.new([])
    var i = ast.count - 1
    while (0 <= i) {
      acc = qq_loop(ast[i], acc)
      i = i - 1
    }
    return acc
  }

  static quasiquote(ast) {
    if (ast is MalList) {
      if (ast.count == 2 && ast[0] is MalSymbol && ast[0].value == "unquote") {
        return ast[1]
      } else {
        return qq_foldr(ast)
      }
    } else if (ast is MalVector) {
      return MalList.new([MalSymbol.new("vec"), qq_foldr(ast)])
    } else if (ast is MalSymbol || ast is MalMap) {
      return MalList.new([MalSymbol.new("quote"), ast])
    } else {
      return ast
    }
  }

  static eval(ast, env) {

    while (true) {
      var tco = false

      var dbgenv = env.find("DEBUG-EVAL")
      if (dbgenv && env.get("DEBUG-EVAL")) {
        System.print("EVAL: %(print(ast))")
      }

    // Process non-list types.
    if (ast is MalSymbol) {
      return env.get(ast.value)
    } else if (ast is MalList) {
      // The only case leading after this switch.
    } else if (ast is MalVector) {
      return MalVector.new(ast.elements.map { |e| eval(e, env) }.toList)
    } else if (ast is MalMap) {
      var m = {}
      for (e in ast.data) {
        m[e.key] = eval(e.value, env)
      }
      return MalMap.new(m)
    } else {
      return ast
    }
    // ast is a list, search for special forms

      if (ast.isEmpty) return ast
      if (ast[0] is MalSymbol) {
        if (ast[0].value == "def!") {
          return env.set(ast[1].value, eval(ast[2], env))
        } else if (ast[0].value == "let*") {
          var letEnv = Env.new(env)
          var i = 0
          while (i < ast[1].count) {
            letEnv.set(ast[1][i].value, eval(ast[1][i + 1], letEnv))
            i = i + 2
          }
          ast = ast[2]
          env = letEnv
          tco = true
        } else if (ast[0].value == "quote") {
          return ast[1]
        } else if (ast[0].value == "quasiquote") {
          ast = quasiquote(ast[1])
          tco = true
        } else if (ast[0].value == "defmacro!") {
          return env.set(ast[1].value, eval(ast[2], env).makeMacro())
        } else if (ast[0].value == "try*") {
          if (ast.count > 2 && ast[2][0] is MalSymbol && ast[2][0].value == "catch*") {
            var fiber = Fiber.new { eval(ast[1], env) }
            var result = fiber.try()
            var error = fiber.error
            if (!error) return result
            if (error == "___MalException___") {
              error = MalException.value
              MalException.set(null)
            }
            return eval(ast[2][2], Env.new(env, [ast[2][1]], [error]))
          } else {
            return eval(ast[1], env)
          }
        } else if (ast[0].value == "do") {
          for (i in 1...(ast.count - 1)) {
            eval(ast[i], env)
          }
          ast = ast[-1]
          tco = true
        } else if (ast[0].value == "if") {
          var condval = eval(ast[1], env)
          if (condval) {
            ast = ast[2]
          } else {
            if (ast.count <= 3) return null
            ast = ast[3]
          }
          tco = true
        } else if (ast[0].value == "fn*") {
          return MalFn.new(ast[2], ast[1].elements, env,
                           Fn.new { |a| eval(ast[2], Env.new(env, ast[1].elements, a)) })
        }
      }
      if (!tco) {
        var f = eval(ast[0], env)
        if (f is MalNativeFn) {
          var args = ast.elements[1..-1].map { |e| eval(e, env) }.toList
          return f.call(args)
        } else if (f is MalFn) {
         if (f.isMacro) {
          ast = f.call(ast.elements[1..-1])
         } else {
          var args = ast.elements[1..-1].map { |e| eval(e, env) }.toList
          ast = f.ast
          env = Env.new(f.env, f.params, args)
         }
        } else {
          Fiber.abort("unknown function type")
        }
      }
    }
  }

  static print(ast) {
    return Printer.pr_str(ast)
  }

  static rep(str) {
    return print(eval(read(str), __repl_env))
  }

  static main() {
    __repl_env = Env.new()
    // core.wren: defined in wren
    for (e in Core.ns) { __repl_env.set(e.key, e.value) }
    __repl_env.set("eval", MalNativeFn.new { |a| eval(a[0], __repl_env) })
    __repl_env.set("*ARGV*", MalList.new(Process.arguments.count > 0 ? Process.arguments[1..-1] : []))
    // core.mal: defined using the language itself
    rep("(def! not (fn* (a) (if a false true)))")
    rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")
    rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")

    if (Process.arguments.count > 0) {
      rep("(load-file \"%(Process.arguments[0])\")")
      return
    }

    while (true) {
      var line = Readline.readLine("user> ")
      if (line == null) break
      if (line != "") {
        var fiber = Fiber.new { System.print(rep(line)) }
        fiber.try()
        var error = fiber.error
        if (error) {
          if (error == "___MalException___") {
            error = Printer.pr_str(MalException.value, false)
            MalException.set(null)
          }
          System.print("Error: %(error)")
        }
      }
    }
    System.print()
  }
}

Mal.main()
