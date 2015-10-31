package mal

fun pr_str(malType: MalType, print_readably: Boolean = false): String =
        if (malType is MalInteger) {
            malType.value.toString()
        } else if (malType is MalKeyword) {
            ":" + malType.value.substring(1)
        } else if (malType is MalString) {
            if (print_readably) {
                "\"" + malType.value.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n") + "\""
            } else malType.value
        } else if (malType is MalConstant) {
            malType.value
        } else if (malType is MalSymbol) {
            malType.value
        } else if (malType is MalFunction) {
            "#" + malType
        } else if (malType is MalCoreException) {
            pr_str(malType.value, print_readably)
        } else if (malType is MalException) {
            "\"" + (malType.message ?: "exception") + "\""
        } else if (malType is MalList) {
            pr_str(malType.elements, "(", ")", print_readably)
        } else if (malType is MalVector) {
            pr_str(malType.elements, "[", "]", print_readably)
        } else if (malType is MalHashMap) {
            malType.elements.map({ it -> pr_str(it, print_readably) }).joinToString(" ", "{", "}")
        } else if (malType is MalAtom) {
            "(atom " + pr_str(malType.value, print_readably) + ")"
        } else {
            throw MalPrinterException("Unrecognized MalType: " + malType)
        }

private fun pr_str(coll: Collection<MalType>, start: String, end: String, print_readably: Boolean = false): String =
        coll.map({ it -> pr_str(it, print_readably) }).joinToString(" ", start, end)

private fun pr_str(mapEntry: Map.Entry<MalString, MalType>, print_readably: Boolean = false): String =
        pr_str(mapEntry.key, print_readably) + " " + pr_str(mapEntry.value, print_readably)
