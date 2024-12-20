import "./env" for Env
import "./readline" for Readline
import "./reader" for MalReader
import "./printer" for Printer
import "./types" for MalSymbol, MalList, MalVector, MalMap

class Mal {
  static read(str) {
    return MalReader.read_str(str)
  }

  static eval(ast, env) {
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
        return eval(ast[2], letEnv)
      }
    }
    var evaled_ast = ast.elements.map { |e| eval(e, env) }.toList
    var f = evaled_ast[0]
    return f.call(evaled_ast[1..-1])
  }

  static print(ast) {
    return Printer.pr_str(ast)
  }

  static rep(str) {
    return print(eval(read(str), __repl_env))
  }

  static main() {
    __repl_env = Env.new()
    __repl_env.set("+", Fn.new { |a| a[0] + a[1] })
    __repl_env.set("-", Fn.new { |a| a[0] - a[1] })
    __repl_env.set("*", Fn.new { |a| a[0] * a[1] })
    __repl_env.set("/", Fn.new { |a| a[0] / a[1] })
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
