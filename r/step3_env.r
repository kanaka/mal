if(!exists("..readline..")) source("readline.r")
if(!exists("..types..")) source("types.r")
if(!exists("..reader..")) source("reader.r")
if(!exists("..printer..")) source("printer.r")
if(!exists("..env..")) source("env.r")

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
    a0sym <- as.character(a0)
    if (a0sym == "def!") {
        res <- EVAL(ast[[3]], env)
        return(Env.set(env, a1, res))
    } else if (a0sym == "let*") {
        let_env <- new.Env(env)
        for(i in seq(1,length(a1),2)) {
            Env.set(let_env, a1[[i]], EVAL(a1[[i+1]], let_env))
        }
        return(EVAL(a2, let_env))
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
Env.set(repl_env, "+", function(a,b) a+b)
Env.set(repl_env, "-", function(a,b) a-b)
Env.set(repl_env, "*", function(a,b) a*b)
Env.set(repl_env, "/", function(a,b) a/b)

rep <- function(str) return(PRINT(EVAL(READ(str), repl_env)))

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
