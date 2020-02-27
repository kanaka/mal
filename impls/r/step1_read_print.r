if(!exists("..readline..")) source("readline.r")
if(!exists("..types..")) source("types.r")
if(!exists("..reader..")) source("reader.r")
if(!exists("..printer..")) source("printer.r")

READ <- function(str) {
    return(read_str(str))
}

EVAL <- function(ast, env) {
    return(ast)
}

PRINT <- function(exp) {
    return(.pr_str(exp, TRUE))
}

rep <- function(str) {
    return(PRINT(EVAL(READ(str), "")))
}

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
