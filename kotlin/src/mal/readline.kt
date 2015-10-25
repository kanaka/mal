package mal

fun raw_readline(prompt: String): String? {
    print(prompt)
    return readLine()
}

fun readline(prompt: String): String? = raw_readline(prompt)
