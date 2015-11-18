package mal

fun read(input: String?): MalType = read_str(input)

fun eval(ast: MalType, env: Env): MalType =
    if (ast is MalList) {
        val first = ast.first()
        if (first is MalSymbol) {
            when (first.value) {
                "def!" -> eval_def_BANG(ast, env)
                "let*" -> eval_let_STAR(ast, env)
                "fn*"  -> eval_fn_STAR(ast, env)
                "do"   -> eval_do(ast, env)
                "if"   -> eval_if(ast, env)
                else   -> eval_function_call(ast, env)
            }
        } else eval_function_call(ast, env)
    } else eval_ast(ast, env)

private fun eval_def_BANG(ast: ISeq, env: Env): MalType =
        env.set(ast.nth(1) as MalSymbol, eval(ast.nth(2), env))

private fun eval_let_STAR(ast: ISeq, env: Env): MalType {
    val child = Env(env)
    val bindings = ast.nth(1) as? ISeq ?: throw MalException("expected sequence as the first parameter to let*")

    val it = bindings.seq().iterator()
    while (it.hasNext()) {
        val key = it.next()
        if (!it.hasNext()) throw MalException("odd number of binding elements in let*")

        val value = eval(it.next(), child)
        child.set(key as MalSymbol, value)
    }

    return eval(ast.nth(2), child)
}

private fun eval_fn_STAR(ast: ISeq, env: Env): MalType {
    val binds = ast.nth(1) as? ISeq ?: throw MalException("fn* requires a binding list as first parameter")
    val symbols = binds.seq().filterIsInstance<MalSymbol>()
    val body = ast.nth(2)

    return MalFunction({ s: ISeq ->
        eval(body, Env(env, symbols, s.seq()))
    })
}

private fun eval_do(ast: ISeq, env: Env): MalType =
        (eval_ast(MalList(ast.rest()), env) as ISeq).seq().last()

private fun eval_if(ast: ISeq, env: Env): MalType {
    val check = eval(ast.nth(1), env)

    return if (check != NIL && check != FALSE) {
        eval(ast.nth(2), env)
    } else if (ast.count() > 3) {
        eval(ast.nth(3), env)
    } else NIL
}

private fun eval_function_call(ast: ISeq, env: Env): MalType {
    val evaluated = eval_ast(ast, env) as ISeq
    val first = evaluated.first() as? MalFunction ?: throw MalException("cannot execute non-function")
    return first.apply(evaluated.rest())
}

fun eval_ast(ast: MalType, env: Env): MalType =
        when (ast) {
            is MalSymbol -> env.get(ast)
            is MalList -> ast.elements.fold(MalList(), { a, b -> a.conj_BANG(eval(b, env)); a })
            is MalVector -> ast.elements.fold(MalVector(), { a, b -> a.conj_BANG(eval(b, env)); a })
            is MalHashMap -> ast.elements.entries.fold(MalHashMap(), { a, b -> a.assoc_BANG(b.key, eval(b.value, env)); a })
            else -> ast
        }

fun print(result: MalType) = pr_str(result, print_readably = true)

fun rep(input: String, env: Env): String =
        print(eval(read(input), env))

fun main(args: Array<String>) {
    val repl_env = Env()
    ns.forEach({ it -> repl_env.set(it.key, it.value) })

    rep("(def! not (fn* (a) (if a false true)))", repl_env)

    while (true) {
        val input = readline("user> ")

        try {
            println(rep(input, repl_env))
        } catch (e: EofException) {
            break
        } catch (e: MalContinue) {
        } catch (e: MalException) {
            println("Error: " + e.message)
        } catch (t: Throwable) {
            println("Uncaught " + t + ": " + t.message)
            t.printStackTrace()
        }
    }
}
