package mal

fun read(input: String?): MalType = read_str(input)

fun eval(ast: MalType, env: Map<String, MalType>): MalType =
    if (ast is MalList) {
        val evaluated = eval_ast(ast, env) as ISeq
        if (evaluated.first() !is MalFunction) throw MalException("cannot execute non-function")
        (evaluated.first() as MalFunction).apply(evaluated.rest())
    } else eval_ast(ast, env)

fun eval_ast(ast: MalType, env: Map<String, MalType>): MalType =
    if (ast is MalSymbol) {
        env.get(ast.value) ?: throw MalException("'${ast.value}' not found")
    } else if (ast is MalList) {
        ast.elements.fold(MalList(), { a, b -> a.conj_BANG(eval(b, env)); a })
    } else if (ast is MalVector) {
        ast.elements.fold(MalVector(), { a, b -> a.conj_BANG(eval(b, env)); a })
    } else if (ast is MalHashMap) {
        ast.elements.entries.fold(MalHashMap(), { a, b -> a.assoc_BANG(b.key, eval(b.value, env)); a })
    } else ast

fun print(result: MalType) = pr_str(result, print_readably = true)

fun main(args: Array<String>) {
    val env = hashMapOf(
            Pair("+", MalFunction({ a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger + y as MalInteger }) })),
            Pair("-", MalFunction({ a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger - y as MalInteger }) })),
            Pair("*", MalFunction({ a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger * y as MalInteger }) })),
            Pair("/", MalFunction({ a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger / y as MalInteger }) }))
    )

    while (true) {
        val input = readline("user> ") ?: break

        try {
            println(print(eval(read(input), env)))
        } catch (e: MalContinue) {
        } catch (e: MalException) {
            println("Error: " + e.message)
        } catch (t: Throwable) {
            println("Uncaught " + t + ": " + t.message)
        }
    }
}
