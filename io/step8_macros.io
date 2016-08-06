MalTypes
MalReader

READ := method(str, MalReader read_str(str))

isPair := method(obj,
    obj ?isSequential and(obj isEmpty not)
)

quasiquote := method(ast,
    if(isPair(ast) not, return(MalList with(list(MalSymbol with("quote"), ast))))
    a0 := ast at(0)
    if(a0 == MalSymbol with("unquote"), return(ast at(1)))
    if(isPair(a0) and (a0 at(0) == MalSymbol with("splice-unquote")),
        return(MalList with(list(MalSymbol with("concat"), a0 at(1), quasiquote(ast rest)))),
        return(MalList with(list(MalSymbol with("cons"), quasiquote(a0), quasiquote(ast rest)))))
)

isMacroCall := method(ast, env,
    if(ast type != "MalList", return false)
    a0 := ast first
    if(a0 type != "MalSymbol", return false)
    if(env find(a0) isNil, return false)
    f := env get(a0)
    (f type == "MalFunc") and (f isMacro)
)

macroexpand := method(ast, env,
    while(isMacroCall(ast, env),
        macro := env get(ast at(0))
        ast = macro blk call(ast rest)
    )
    ast
)

eval_ast := method(ast, env,
   (ast type) switch(
       "MalSymbol", env get(ast),
       "MalList", MalList with(ast map(a, EVAL(a, env))),
       "MalVector", MalVector with(ast map(a, EVAL(a, env))),
       "MalMap",
           m := MalMap clone
           ast foreach(k, v,
               keyObj := MalMap keyToObj(k)
               m atPut(MalMap objToKey(EVAL(keyObj, env)), EVAL(v, env))
           )
           m,
       ast
   )
)

EVAL := method(ast, env,
    loop(
        if(ast type != "MalList", return(eval_ast(ast, env)))

        ast = macroexpand(ast, env)
        if(ast type != "MalList", return(eval_ast(ast, env)))
        if(ast isEmpty, return ast)

        if(ast at(0) type == "MalSymbol",
            ast at(0) val switch(
                "def!",
                    return(env set(ast at(1), EVAL(ast at(2), env))),
                "do",
                    eval_ast(ast slice(1,-1), env)
                    ast = ast last
                    continue, // TCO
                "if",
                    ast = if(EVAL(ast at(1), env), ast at(2), ast at(3))
                    continue, // TCO
                "fn*",
                    return(MalFunc with(ast at(2), ast at(1), env, block(a, EVAL(ast at(2), Env with(env, ast at(1), a))))),
                "let*",
                    letEnv := Env with(env)
                    varName := nil
                    ast at(1) foreach(i, e,
                        if(i % 2 == 0,
                            varName := e,
                            letEnv set(varName, EVAL(e, letEnv))
                        )
                    )
                    ast = ast at(2)
                    env = letEnv
                    continue, // TCO
                "quote",
                    return(ast at(1)),
                "quasiquote",
                    ast = quasiquote(ast at(1))
                    continue, // TCO
                "defmacro!",
                    return(env set(ast at(1), EVAL(ast at(2), env) setIsMacro(true))),
                "macroexpand",
                    return(macroexpand(ast at(1), env))
            )
        )

        // Apply
        el := eval_ast(ast, env)
        f := el at(0)
        args := el rest
        f type switch(
            "Block",
                return(f call(args)),
            "MalFunc",
                ast = f ast
                env = Env with(f env, f params, args)
                continue, // TCO
            Exception raise("Unknown function type")
        )
    )
)

PRINT := method(exp, exp malPrint(true))

repl_env := Env with(nil)

RE := method(str, EVAL(READ(str), repl_env))

REP := method(str, PRINT(RE(str)))

MalCore NS foreach(k, v, repl_env set(MalSymbol with(k), v))
repl_env set(MalSymbol with("eval"), block(a, EVAL(a at(0), repl_env)))
repl_env set(MalSymbol with("*ARGV*"), MalList with(System args slice(2)))

// core.mal: defined using the language itself
RE("(def! not (fn* (a) (if a false true)))")
RE("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))")
RE("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")
RE("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))")

if(System args size > 1,
    REP("(load-file \"" .. (System args at(1)) .. "\")")
    System exit(0)
)

loop(
    line := MalReadline readLine("user> ")
    if(line isNil, break)
    if(line isEmpty, continue)
    e := try(REP(line) println)
    e catch(Exception,
        ("Error: " .. (e error)) println
    )
)
