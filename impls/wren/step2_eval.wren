import "./readline" for Readline
import "./reader" for MalReader
import "./printer" for Printer
import "./types" for MalSymbol, MalList, MalVector, MalMap

class Mal {
  static read(str) {
    return MalReader.read_str(str)
  }

  static eval_ast(ast, env) {
    if (ast is MalSymbol) {
      if (!env.containsKey(ast.value)) Fiber.abort("'%(ast.value)' not found")
      return env[ast.value]
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
    if (!(ast is MalList)) return eval_ast(ast, env)
    if (ast.isEmpty) return ast
    var evaled_ast = eval_ast(ast, env)
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
    __repl_env = {
      "+": Fn.new { |a| a[0] + a[1] },
      "-": Fn.new { |a| a[0] - a[1] },
      "*": Fn.new { |a| a[0] * a[1] },
      "/": Fn.new { |a| a[0] / a[1] }
    }
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
