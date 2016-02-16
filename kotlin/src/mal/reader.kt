package mal

import kotlin.text.Regex

val TOKEN_REGEX = Regex("[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;.*|[^\\s\\[\\]{}('\"`,;)]*)")
val ATOM_REGEX = Regex("(^-?[0-9]+$)|(^nil$)|(^true$)|(^false$)|^\"(.*)\"$|:(.*)|(^[^\"]*$)")

class Reader(sequence: Sequence<String>) {
    val tokens = sequence.iterator()
    var current = advance()

    fun next(): String? {
        var result = current
        current = advance()
        return result
    }

    fun peek(): String? = current

    private fun advance(): String? = if (tokens.hasNext()) tokens.next() else null
}

fun read_str(input: String?): MalType {
    val tokens = tokenizer(input) ?: return NIL
    return read_form(Reader(tokens))
}

fun tokenizer(input: String?): Sequence<String>? {
    if (input == null) return null

    return TOKEN_REGEX.findAll(input)
            .map({ it -> it.groups[1]?.value as String })
            .filter({ it != "" && !it.startsWith(";")})
}

fun read_form(reader: Reader): MalType =
        when (reader.peek()) {
            null -> throw MalContinue()
            "("  -> read_list(reader)
            ")"  -> throw MalReaderException("expected form, got ')'")
            "["  -> read_vector(reader)
            "]"  -> throw MalReaderException("expected form, got ']'")
            "{"  -> read_hashmap(reader)
            "}"  -> throw MalReaderException("expected form, got '}'")
            "'"  -> read_shorthand(reader, "quote")
            "`"  -> read_shorthand(reader, "quasiquote")
            "~"  -> read_shorthand(reader, "unquote")
            "~@" -> read_shorthand(reader, "splice-unquote")
            "^"  -> read_with_meta(reader)
            "@"  -> read_shorthand(reader, "deref")
            else -> read_atom(reader)
        }

fun read_list(reader: Reader): MalType = read_sequence(reader, MalList(), ")")
fun read_vector(reader: Reader): MalType = read_sequence(reader, MalVector(), "]")

private fun read_sequence(reader: Reader, sequence: IMutableSeq, end: String): MalType {
    reader.next()

    do {
        val form = when (reader.peek()) {
            null -> throw MalReaderException("expected '$end', got EOF")
            end  -> { reader.next(); null }
            else -> read_form(reader)
        }

        if (form != null) {
            sequence.conj_BANG(form)
        }
    } while (form != null)

    return sequence
}

fun read_hashmap(reader: Reader): MalType {
    reader.next()
    val hashMap = MalHashMap()

    do {
        var value : MalType? = null;
        val key = when (reader.peek()) {
            null -> throw MalReaderException("expected '}', got EOF")
            "}"  -> { reader.next(); null }
            else -> {
                var key = read_form(reader)
                if (key !is MalString) {
                    throw MalReaderException("hash-map keys must be strings or keywords")
                }
                value = when (reader.peek()) {
                    null -> throw MalReaderException("expected form, got EOF")
                    else -> read_form(reader)
                }
                key
            }
        }

        if (key != null) {
            hashMap.assoc_BANG(key, value as MalType)
        }
    } while (key != null)

    return hashMap
}

fun read_shorthand(reader: Reader, symbol: String): MalType {
    reader.next()

    val list = MalList()
    list.conj_BANG(MalSymbol(symbol))
    list.conj_BANG(read_form(reader))

    return list
}

fun read_with_meta(reader: Reader): MalType {
    reader.next()

    val meta = read_form(reader)
    val obj = read_form(reader)

    val list = MalList()
    list.conj_BANG(MalSymbol("with-meta"))
    list.conj_BANG(obj)
    list.conj_BANG(meta)

    return list
}

fun read_atom(reader: Reader): MalType {
    val next = reader.next() ?: throw MalReaderException("Unexpected null token")
    val groups = ATOM_REGEX.find(next)?.groups ?: throw MalReaderException("Unrecognized token: " + next)

    return if (groups[1]?.value != null) {
        MalInteger(groups[1]?.value?.toLong() ?: throw MalReaderException("Error parsing number: " + next))
    } else if (groups[2]?.value != null) {
        NIL
    } else if (groups[3]?.value != null) {
        TRUE
    } else if (groups[4]?.value != null) {
        FALSE
    } else if (groups[5]?.value != null) {
        MalString((groups[5]?.value as String).replace("\\n", "\n").replace("\\\"", "\"").replace("\\\\", "\\"))
    } else if (groups[6]?.value != null) {
        MalKeyword(groups[6]?.value as String)
    } else if (groups[7]?.value != null) {
        MalSymbol(groups[7]?.value as String)
    } else {
        throw MalReaderException("Unrecognized token: " + next)
    }
}
