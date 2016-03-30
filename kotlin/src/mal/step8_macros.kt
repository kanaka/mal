package mal

import java.util.*

fun read(input: String?): MalType = read_str(input)

fun eval(_ast: MalType, _env: Env): MalType {
    var ast = _ast
    var env = _env

    while (true) {
        ast = macroexpand(ast, env)

        if (ast is MalList) {
            if (ast.count() == 0) return ast
            when ((ast.first() as? MalSymbol)?.value) {
                "def!" -> return env.set(ast.nth(1) as MalSymbol, eval(ast.nth(2), env))
                "let*" -> {
                    val childEnv = Env(env)
                    val bindings = ast.nth(1) as? ISeq ?: throw MalException("expected sequence as the first parameter to let*")

                    val it = bindings.seq().iterator()
                    while (it.hasNext()) {
                        val key = it.next()
                        if (!it.hasNext()) throw MalException("odd number of binding elements in let*")
                        childEnv.set(key as MalSymbol, eval(it.next(), childEnv))
                    }

                    env = childEnv
                    ast = ast.nth(2)
                }
                "fn*" -> return fn_STAR(ast, env)
                "do" -> {
                    eval_ast(ast.slice(1, ast.count() - 1), env)
                    ast = ast.seq().last()
                }
                "if" -> {
                    val check = eval(ast.nth(1), env)

                    if (check !== NIL && check !== FALSE) {
                        ast = ast.nth(2)
                    } else if (ast.count() > 3) {
                        ast = ast.nth(3)
                    } else return NIL
                }
                "quote" -> return ast.nth(1)
                "quasiquote" -> ast = quasiquote(ast.nth(1))
                "defmacro!" -> return defmacro(ast, env)
                "macroexpand" -> return macroexpand(ast.nth(1), env)
                else -> {
                    val evaluated = eval_ast(ast, env) as ISeq
                    val firstEval = evaluated.first()

                    when (firstEval) {
                        is MalFnFunction -> {
                            ast = firstEval.ast
                            env = Env(firstEval.env, firstEval.params, evaluated.rest().seq())
                        }
                        is MalFunction -> return firstEval.apply(evaluated.rest())
                        else -> throw MalException("cannot execute non-function")
                    }
                }
            }
        } else return eval_ast(ast, env)
    }
}

fun eval_ast(ast: MalType, env: Env): MalType =
        when (ast) {
            is MalSymbol -> env.get(ast)
            is MalList -> ast.elements.fold(MalList(), { a, b -> a.conj_BANG(eval(b, env)); a })
            is MalVector -> ast.elements.fold(MalVector(), { a, b -> a.conj_BANG(eval(b, env)); a })
            is MalHashMap -> ast.elements.entries.fold(MalHashMap(), { a, b -> a.assoc_BANG(b.key, eval(b.value, env)); a })
            else -> ast
        }

private fun fn_STAR(ast: MalList, env: Env): MalType {
    val binds = ast.nth(1) as? ISeq ?: throw MalException("fn* requires a binding list as first parameter")
    val params = binds.seq().filterIsInstance<MalSymbol>()
    val body = ast.nth(2)

    return MalFnFunction(body, params, env, { s: ISeq -> eval(body, Env(env, params, s.seq())) })
}

private fun is_pair(ast: MalType): Boolean = ast is ISeq && ast.seq().any()

private fun quasiquote(ast: MalType): MalType {
    if (!is_pair(ast)) {
        val quoted = MalList()
        quoted.conj_BANG(MalSymbol("quote"))
        quoted.conj_BANG(ast)
        return quoted
    }

    val seq = ast as ISeq
    var first = seq.first()

    if ((first as? MalSymbol)?.value == "unquote") {
        return seq.nth(1)
    }

    if (is_pair(first) && ((first as ISeq).first() as? MalSymbol)?.value == "splice-unquote") {
        val spliced = MalList()
        spliced.conj_BANG(MalSymbol("concat"))
        spliced.conj_BANG(first.nth(1))
        spliced.conj_BANG(quasiquote(MalList(seq.seq().drop(1).toCollection(LinkedList<MalType>()))))
        return spliced
    }

    val consed = MalList()
    consed.conj_BANG(MalSymbol("cons"))
    consed.conj_BANG(quasiquote(ast.first()))
    consed.conj_BANG(quasiquote(MalList(seq.seq().drop(1).toCollection(LinkedList<MalType>()))))
    return consed
}

private fun is_macro_call(ast: MalType, env: Env): Boolean {
    val ast_list = ast as? MalList ?: return false
    if (ast_list.count() == 0) return false
    val symbol = ast_list.first() as? MalSymbol ?: return false
    val function = env.find(symbol) as? MalFunction ?: return false

    return function.is_macro
}

private fun macroexpand(_ast: MalType, env: Env): MalType {
    var ast = _ast
    while (is_macro_call(ast, env)) {
        val symbol = (ast as MalList).first() as MalSymbol
        val function = env.find(symbol) as MalFunction
        ast = function.apply(ast.rest())
    }
    return ast
}

private fun defmacro(ast: MalList, env: Env): MalType {
    val macro = eval(ast.nth(2), env) as MalFunction
    macro.is_macro = true

    return env.set(ast.nth(1) as MalSymbol, macro)
}

fun print(result: MalType) = pr_str(result, print_readably = true)

fun rep(input: String, env: Env): String =
        print(eval(read(input), env))

fun main(args: Array<String>) {
    val repl_env = Env()
    ns.forEach({ it -> repl_env.set(it.key, it.value) })

    repl_env.set(MalSymbol("*ARGV*"), MalList(args.drop(1).map({ it -> MalString(it) }).toCollection(LinkedList<MalType>())))
    repl_env.set(MalSymbol("eval"), MalFunction({ a: ISeq -> eval(a.first(), repl_env) }))

    rep("(def! not (fn* (a) (if a false true)))", repl_env)
    rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))", repl_env)
    rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))", repl_env)
    rep("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))", repl_env)

    if (args.any()) {
        rep("(load-file \"${args[0]}\")", repl_env)
        return
    }

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
