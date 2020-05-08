import "./readline" for Readline
import "./reader" for MalReader
import "./printer" for Printer

class Mal {
  static read(str) {
    return MalReader.read_str(str)
  }

  static eval(ast, env) {
    return ast
  }

  static print(ast) {
    return Printer.pr_str(ast)
  }

  static rep(str) {
    return print(eval(read(str), null))
  }

  static main() {
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
