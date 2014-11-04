..core.. <- TRUE

if(!exists("..types..")) source("types.r")
if(!exists("..printer..")) source("printer.r")


# String functions

pr_str <- function(...) .pr_list(..., print_readably=TRUE, join=" ")

str <- function(...) .pr_list(..., print_readably=FALSE, join="")

prn <- function(...) {
    cat(.pr_list(..., print_readably=TRUE, join=" ")); cat("\n")
    nil
}

println <- function(...) {
    cat(.pr_list(..., print_readably=FALSE, join=" ")); cat("\n")
    nil
}

# Sequence functions
cons <- function(a,b) {
    new_lst <- append(list(a), b)
    class(new_lst) <- "List"
    new_lst
}

do_concat <- function(...) {
    new_lst <- list()
    for(l in list(...)) {
        new_lst <- append(new_lst, l)
    }
    class(new_lst) <- "List"
    new_lst
}

core_ns <- list(
    "="=function(a,b) .equal_q(a,b),

    "pr-str"=pr_str,
    "str"=str,
    "prn"=prn,
    "println"=println,
    "read-string"=function(str) read_str(str),
    "slurp"=function(path) readChar(path, file.info(path)$size),
    "<"=function(a,b) a<b,
    "<="=function(a,b) a<=b,
    ">"=function(a,b) a>b,
    ">="=function(a,b) a>=b,
    "+"=function(a,b) a+b,
    "-"=function(a,b) a-b,
    "*"=function(a,b) a*b,
    "/"=function(a,b) a/b,

    "list"=function(...) new.list(...),
    "list?"=function(a) .list_q(a),
    "empty?"=function(a) .sequential_q(a) && length(a) == 0,
    "count"=function(a) length(a),

    "cons"=cons,
    "concat"=do_concat
)
