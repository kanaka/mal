package mal

fun pr_str(malType: MalType, print_readably: Boolean = false): String =
        when (malType) {
            is MalInteger -> malType.value.toString()
            is MalKeyword -> ":" + malType.value.substring(1)
            is MalString ->
                if (print_readably) {
                    "\"" + malType.value.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n") + "\""
                } else malType.value
            is MalConstant -> malType.value
            is MalSymbol -> malType.value
            is MalFunction -> "#" + malType
            is MalCoreException -> pr_str(malType.value, print_readably)
            is MalException -> "\"" + (malType.message ?: "exception") + "\""
            is MalList -> pr_str(malType.elements, "(", ")", print_readably)
            is MalVector -> pr_str(malType.elements, "[", "]", print_readably)
            is MalHashMap -> malType.elements.map({ it -> pr_str(it, print_readably) }).joinToString(" ", "{", "}")
            is MalAtom -> "(atom " + pr_str(malType.value, print_readably) + ")"
            else -> throw MalPrinterException("Unrecognized MalType: " + malType)
        }

private fun pr_str(coll: Collection<MalType>, start: String, end: String, print_readably: Boolean = false): String =
        coll.map({ it -> pr_str(it, print_readably) }).joinToString(" ", start, end)

private fun pr_str(mapEntry: Map.Entry<MalString, MalType>, print_readably: Boolean = false): String =
        pr_str(mapEntry.key, print_readably) + " " + pr_str(mapEntry.value, print_readably)
