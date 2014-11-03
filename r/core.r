..core.. <- TRUE

if(!exists("..types..")) source("types.r")
if(!exists("..printer..")) source("printer.r")


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

core_ns <- list(
    "="=function(a,b) .equal_q(a,b),

    "pr-str"=pr_str,
    "str"=str,
    "prn"=prn,
    "println"=println,
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
    "count"=function(a) length(a)

)
