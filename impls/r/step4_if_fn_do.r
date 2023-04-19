if(!exists("..readline..")) source("readline.r")
if(!exists("..types..")) source("types.r")
if(!exists("..reader..")) source("reader.r")
if(!exists("..printer..")) source("printer.r")
if(!exists("..env..")) source("env.r")
if(!exists("..core..")) source("core.r")

READ <- function(str) {
    return(read_str(str))
}

EVAL <- function(ast, env) {

    dbgevalenv <- Env.find(env, "DEBUG-EVAL")
    if (!.nil_q(dbgevalenv)) {
        dbgeval <- Env.get(dbgevalenv, "DEBUG-EVAL")
        if (!.nil_q(dbgeval) && !identical(dbgeval, FALSE))
            cat("EVAL: ", .pr_str(ast,TRUE), "\n", sep="")
    }

    if (.symbol_q(ast)) {
        return(Env.get(env, ast))
    } else if (.list_q(ast)) {
        # exit this switch
    } else if (.vector_q(ast)) {
        return(new.vectorl(lapply(ast, function(a) EVAL(a, env))))
    } else if (.hash_map_q(ast)) {
        lst <- list()
        for(k in ls(ast)) {
            lst[[length(lst)+1]] = k
            lst[[length(lst)+1]] = EVAL(ast[[k]], env)
        }
        return(new.hash_mapl(lst))
    } else {
        return(ast)
    }

    if (length(ast) == 0) { return(ast) }

    # apply list
    switch(paste("l",length(ast),sep=""),
           l0={ return(ast) },
           l1={ a0 <- ast[[1]]; a1 <- NULL;     a2 <- NULL },
           l2={ a0 <- ast[[1]]; a1 <- ast[[2]]; a2 <- NULL },
              { a0 <- ast[[1]]; a1 <- ast[[2]]; a2 <- ast[[3]] })
    if (length(a0) > 1) a0sym <- "__<*fn*>__"
    else                a0sym <- as.character(a0)
    if (a0sym == "def!") {
        res <- EVAL(ast[[3]], env)
        return(Env.set(env, a1, res))
    } else if (a0sym == "let*") {
        let_env <- new.Env(env)
        for(i in seq(1,length(a1),2)) {
            Env.set(let_env, a1[[i]], EVAL(a1[[i+1]], let_env))
        }
        return(EVAL(a2, let_env))
    } else if (a0sym == "do") {
        if (2 < length(ast))
            for(i in seq(2, length(ast) - 1))
                EVAL(ast[[i]], env)
        return(EVAL(ast[[length(ast)]], env))
    } else if (a0sym == "if") {
        cond <- EVAL(a1, env)
        if (.nil_q(cond) || identical(cond, FALSE)) {
            if (length(ast) < 4) return(nil)
            return(EVAL(ast[[4]], env))
        } else {
            return(EVAL(a2, env))
        }
    } else if (a0sym == "fn*") {
        return(function(...) {
            EVAL(a2, new.Env(env, a1, list(...)))
        })
    } else {
        f <- EVAL(a0, env)
        args <- new.listl(lapply(slice(ast, 2), function(a) EVAL(a, env)))
        return(do.call(f, args))
    }
}

PRINT <- function(exp) {
    return(.pr_str(exp, TRUE))
}

repl_env <- new.Env()
rep <- function(str) return(PRINT(EVAL(READ(str), repl_env)))

# core.r: defined using R
for(k in names(core_ns)) { Env.set(repl_env, k, core_ns[[k]]) }

# core.mal: defined using the language itself
. <- rep("(def! not (fn* (a) (if a false true)))")


repeat {
    line <- readline("user> ")
    if (is.null(line)) { cat("\n"); break }
    tryCatch({
        cat(rep(line),"\n", sep="")
    }, error=function(err) {
        cat("Error: ", get_error(err),"\n", sep="")
    })
    # R debug/fatal with tracebacks:
    #cat(rep(line),"\n", sep="")
}
