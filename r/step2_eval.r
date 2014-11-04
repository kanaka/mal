if(!exists("..readline..")) source("readline.r")
if(!exists("..types..")) source("types.r")
if(!exists("..reader..")) source("reader.r")
if(!exists("..printer..")) source("printer.r")

READ <- function(str) {
    return(read_str(str))
}

eval_ast <- function(ast, env) {
    if (.symbol_q(ast)) {
        env[[as.character(ast)]]
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
    el <- eval_ast(ast, env)
    f <- el[[1]]
    return(do.call(f,el[-1]))
}

PRINT <- function(exp) {
    return(.pr_str(exp, TRUE))
}

repl_env <- new.env()
repl_env[["+"]] <- function(a,b) a+b
repl_env[["-"]] <- function(a,b) a-b
repl_env[["*"]] <- function(a,b) a*b
repl_env[["/"]] <- function(a,b) a/b

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
