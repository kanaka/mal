..printer.. <- TRUE

if(!exists("..types..")) source("types.r")

.pr_list <- function(lst, print_readably=TRUE, join="") {
    concatl(lapply(lst,
                   function(e) .pr_str(e, print_readably)), sep=join)
}

.pr_str <- function(exp, print_readably=TRUE) {
    pr <- print_readably
    switch(class(exp),
        "List"={
            paste("(", .pr_list(exp, pr, " "), ")", sep="", collapse="")
        },
        "Vector"={
            paste("[", .pr_list(exp, pr, " "), "]", sep="", collapse="")
        },
        "HashMap"={
            hlst <- list()
            if (length(exp) > 0) {
                for(k in ls(exp)) {
                    hlst[[length(hlst)+1]] <- k
                    hlst[[length(hlst)+1]] <- exp[[k]]
                }
            }
            paste("{", .pr_list(hlst, pr, " "), "}", sep="", collapse="")
        },
        "character"={
            if (substring(exp,1,1) == "\u029e") {
                concat(":", substring(exp,2))
            } else if (print_readably) {
                paste("\"",
                      gsub("\\n", "\\\\n",
                           gsub("\\\"", "\\\\\"",
                                gsub("\\\\", "\\\\\\\\", exp))),
                      "\"", sep="", collapse="")
            } else {
                exp
            }
        },
        "Symbol"={ exp },
        "nil"={ "nil" },
        "logical"={ tolower(exp) },
        "MalFunc"={
            paste("(fn* ", .pr_str(exp$params,TRUE),
                  " ", .pr_str(exp$ast, TRUE), ")", sep="")
        },
        "function"={ "<#function>" },
        "Atom"={
            paste("(atom ", .pr_str(exp$val,TRUE), ")", sep="")
        },
        { toString(exp) })
}
