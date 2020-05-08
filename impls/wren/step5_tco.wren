import "./env" for Env
import "./readline" for Readline
import "./reader" for MalReader
import "./printer" for Printer
import "./types" for MalSymbol, MalList, MalVector, MalMap, MalNativeFn, MalFn
import "./core" for Core

class Mal {
  static read(str) {
    return MalReader.read_str(str)
  }

  static eval_ast(ast, env) {
    if (ast is MalSymbol) {
      return env.get(ast.value)
    } else if (ast is MalList) {
      return MalList.new(ast.elements.map { |e| eval(e, env) }.toList)
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
  }

  static eval(ast, env) {
    while (true) {
      var tco = false
      if (!(ast is MalList)) return eval_ast(ast, env)
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
        var evaled_ast = eval_ast(ast, env)
        var f = evaled_ast[0]
        if (f is MalNativeFn) {
          return f.call(evaled_ast[1..-1])
        } else if (f is MalFn) {
          ast = f.ast
          env = Env.new(f.env, f.params, evaled_ast[1..-1])
          tco = true
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
    // core.mal: defined using the language itself
    rep("(def! not (fn* (a) (if a false true)))")
    while (true) {
      var line = Readline.readLine("user> ")
      if (line == null) break
      if (line != "") {
        var fiber = Fiber.new { System.print(rep(line)) }
        fiber.try()
        if (fiber.error) System.print("Error: %(fiber.error)")
      }
    }
    System.print()
  }
}

Mal.main()
