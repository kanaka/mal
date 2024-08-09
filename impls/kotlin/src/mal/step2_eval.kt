package mal

fun read(input: String?): MalType = read_str(input)

fun eval(ast: MalType, env: Map<String, MalType>): MalType {
    // println ("EVAL: ${print(ast)}")
    when (ast) {
        is MalList -> {
            if (ast.count() == 0) return ast
            val evaluated = ast.elements.fold(MalList(), { a, b -> a.conj_BANG(eval(b, env)); a })
            if (evaluated.first() !is MalFunction) throw MalException("cannot execute non-function")
            return (evaluated.first() as MalFunction).apply(evaluated.rest())
        }
        is MalSymbol -> return env[ast.value] ?: throw MalException("'${ast.value}' not found")
        is MalVector -> return ast.elements.fold(MalVector(), { a, b -> a.conj_BANG(eval(b, env)); a })
        is MalHashMap -> return ast.elements.entries.fold(MalHashMap(), { a, b -> a.assoc_BANG(b.key, eval(b.value, env)); a })
        else -> return ast
    }
}

fun print(result: MalType) = pr_str(result, print_readably = true)

fun main(args: Array<String>) {
    val env = hashMapOf(
            Pair("+", MalFunction({ a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger + y as MalInteger }) })),
            Pair("-", MalFunction({ a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger - y as MalInteger }) })),
            Pair("*", MalFunction({ a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger * y as MalInteger }) })),
            Pair("/", MalFunction({ a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger / y as MalInteger }) }))
    )

    while (true) {
        val input = readline("user> ")

        try {
            println(print(eval(read(input), env)))
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
