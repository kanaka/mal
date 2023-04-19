if(!exists("..readline..")) source("readline.r")
if(!exists("..types..")) source("types.r")
if(!exists("..reader..")) source("reader.r")
if(!exists("..printer..")) source("printer.r")

READ <- function(str) {
    return(read_str(str))
}

EVAL <- function(ast, env) {

    # cat("EVAL: ", .pr_str(ast,TRUE), "\n", sep="")

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

    # apply list
    if (length(ast) == 0) {
        return(ast)
    }
    f <- EVAL(ast[[1]], env)
    args <- new.listl(lapply(slice(ast, 2), function(a) EVAL(a, env)))
    return(do.call(f, args))
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
