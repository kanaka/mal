..printer.. <- TRUE

if(!exists("..types..")) source("types.r")

.pr_list <- function(..., print_readably=TRUE, join="") {
    concatl(lapply(list(...),
                   function(e) .pr_str(e, print_readably)), sep=join)
}

.pr_str <- function(exp, print_readably=TRUE) {
    pr <- print_readably
    switch(class(exp),
        "List"={
            data <- paste(lapply(exp, function(e) .pr_str(e, pr)),
                          sep="", collapse=" ")
            paste("(", data, ")", sep="", collapse="")
        },
        "Vector"={
            data <- paste(lapply(exp, function(e) .pr_str(e, pr)),
                          sep=" ", collapse=" ")
            paste("[", data, "]", sep="", collapse="")
        },
        "character"={
            if (print_readably) {
                paste("\"",
                      gsub("\\n", "\\\\n",
                           gsub("\\\"", "\\\\\"",
                                gsub("\\\\", "\\\\\\\\", exp))),
                      "\"", sep="", collapse="")
            } else {
                exp
            }
        },
        "nil"={ "nil" },
        "logical"={ tolower(exp) },
        "MalFunc"={
            paste("(fn* ", .pr_str(exp$params,TRUE),
                  " ", .pr_str(exp$ast, FALSE), ")", sep="")
        },
        "function"={ "<#function>" },
        { toString(exp) })
}


