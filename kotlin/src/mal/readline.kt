package mal

class EofException : Exception("EOF")

fun readline(prompt: String): String {
    print(prompt)
    return readLine() ?: throw EofException()
}
