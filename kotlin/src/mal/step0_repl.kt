package mal

fun main(args: Array<String>) {
    fun read(input: String?): String? = input
    fun eval(expression: String?): String? = expression
    fun print(result: String?): String? = result

    while (true) {
        val input = readline("user> ")

        try {
            println(print(eval(read(input))))
        } catch (e: EofException) {
            break
        } catch (t: Throwable) {
            println("Uncaught " + t + ": " + t.message)
        }
    }
}
