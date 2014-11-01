source("readline.r")

READ <- function(str) {
    return(str)
}

EVAL <- function(ast, env) {
    return(ast)
}

PRINT <- function(exp) {
    return(exp)
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
        cat("Error: ", err$message,"\n", sep="")
    })
}
