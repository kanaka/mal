package mal

fun read(input: String?): MalType = read_str(input)

fun eval(ast: MalType, env: Env): MalType {

    val dbgeval = env.get("DEBUG-EVAL")
    if (dbgeval !== null && dbgeval !== NIL && dbgeval !== FALSE) {
        println ("EVAL: ${print(ast)}")
    }

    when (ast) {
        is MalList -> {
            if (ast.count() == 0) return ast
            val first = ast.first()
            if (first is MalSymbol && first.value == "def!") {
                return env.set(ast.nth(1) as MalSymbol, eval(ast.nth(2), env))
            } else if (first is MalSymbol && first.value == "let*") {
                val child = Env(env)
                val bindings = ast.nth(1)
                if (bindings !is ISeq) throw MalException("expected sequence as the first parameter to let*")
                val it = bindings.seq().iterator()
                while (it.hasNext()) {
                    val key = it.next()
                    if (!it.hasNext()) throw MalException("odd number of binding elements in let*")
                    val value = eval(it.next(), child)
                    child.set(key as MalSymbol, value)
                }
                return eval(ast.nth(2), child)
            } else {
                val evaluated = ast.elements.fold(MalList(), { a, b -> a.conj_BANG(eval(b, env)); a })
                if (evaluated.first() !is MalFunction) throw MalException("cannot execute non-function")
                return (evaluated.first() as MalFunction).apply(evaluated.rest())
            }
        }
        is MalSymbol -> return env.get(ast.value) ?: throw MalException("'${ast.value}' not found")
        is MalVector -> return ast.elements.fold(MalVector(), { a, b -> a.conj_BANG(eval(b, env)); a })
        is MalHashMap -> return ast.elements.entries.fold(MalHashMap(), { a, b -> a.assoc_BANG(b.key, eval(b.value, env)); a })
        else -> return ast
    }
}

fun print(result: MalType) = pr_str(result, print_readably = true)

fun main(args: Array<String>) {
    val env = Env()
    env.set(MalSymbol("+"), MalFunction({ a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger + y as MalInteger }) }))
    env.set(MalSymbol("-"), MalFunction({ a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger - y as MalInteger }) }))
    env.set(MalSymbol("*"), MalFunction({ a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger * y as MalInteger }) }))
    env.set(MalSymbol("/"), MalFunction({ a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger / y as MalInteger }) }))

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
