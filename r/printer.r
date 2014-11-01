..printer.. <- TRUE

if(!exists("..types..")) source("types.r")

.pr_str <- function(exp, print_readably=TRUE) {
    #cat("-", class(exp), as.character(exp), "\n")
    switch(class(exp),
        "List"={
            data <- paste(lapply(exp, function(e) .pr_str(e)),
                          sep="", collapse=" ")
            paste("(", data, ")", sep="", collapse="")
        },
        "Vector"={
            data <- paste(lapply(exp, function(e) .pr_str(e)),
                          sep=" ", collapse=" ")
            paste("[", data, "]", sep="", collapse="")
        },
        "character"={
            if (print_readably) {
                paste("\"", exp, "\"", sep="", collapse="" )
            } else {
                exp
            }
        },
        "NULL"={ "nil" },
        "logical"={ tolower(exp) },
        "function"={ "<#function>" },
        { toString(exp) })
}


