import "io" for Stdin, Stdout

class Readline {
  static readLine(prompt) {
    var line = null
    var fiber = Fiber.new {
      System.write(prompt)
      Stdout.flush()
      line = Stdin.readLine()
    }
    var error = fiber.try()
    return error ? null : line
  }
}
