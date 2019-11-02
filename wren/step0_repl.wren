import "./readline" for Readline

class Mal {
  static read(str) {
    return str
  }

  static eval(ast, env) {
    return ast
  }

  static print(ast) {
    return ast
  }

  static rep(str) {
    return print(eval(read(str), null))
  }

  static main() {
    while (true) {
      var line = Readline.readLine("user> ")
      if (line == null) break
      if (line != "") System.print(rep(line))
    }
    System.print()
  }
}

Mal.main()
