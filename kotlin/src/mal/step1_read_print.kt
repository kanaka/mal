package mal

fun main(args: Array<String>) {
    fun read(input: String?): MalType = read_str(input)
    fun eval(expression: MalType): MalType = expression
    fun print(result: MalType) = pr_str(result, print_readably = true)

    while (true) {
        val input = readline("user> ")

        try {
            println(print(eval(read(input))))
        } catch (e: EofException) {
            break
        } catch (e: MalContinue) {
        } catch (e: MalException) {
            println("Error: " + e.message)
        } catch (t: Throwable) {
            println("Uncaught " + t + ": " + t.message)
        }
    }
}
