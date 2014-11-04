if(!exists("..readline..")) source("readline.r")
if(!exists("..types..")) source("types.r")
if(!exists("..reader..")) source("reader.r")
if(!exists("..printer..")) source("printer.r")
if(!exists("..env..")) source("env.r")
if(!exists("..core..")) source("core.r")

READ <- function(str) {
    return(read_str(str))
}

eval_ast <- function(ast, env) {
    if (.symbol_q(ast)) {
        Env.get(env, ast)
    } else if (.list_q(ast)) {
        new.listl(lapply(ast, function(a) EVAL(a, env)))
    } else if (.vector_q(ast)) {
        new.vectorl(lapply(ast, function(a) EVAL(a, env)))
    } else if (.hash_map_q(ast)) {
        lst <- list()
        for(k in ls(ast)) {
            lst[[length(lst)+1]] = k
            lst[[length(lst)+1]] = EVAL(ast[[k]], env)
        }
        new.hash_mapl(lst)
    } else {
        ast
    }
}

EVAL <- function(ast, env) {
    #cat("EVAL: ", .pr_str(ast,TRUE), "\n", sep="")
    if (!.list_q(ast)) {
        return(eval_ast(ast, env))
    }

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
        el <- eval_ast(slice(ast,2), env)
        return(el[[length(el)]])
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
        el <- eval_ast(ast, env)
        f <- el[[1]]
        return(do.call(f,slice(el,2)))
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
