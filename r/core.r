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
    new.listl(new_lst)
}

do_concat <- function(...) {
    new_lst <- list()
    for(l in list(...)) {
        new_lst <- append(new_lst, l)
    }
    new.listl(new_lst)
}

do_apply <- function(f, ...) {
    p <- list(...)
    args <- list()
    if (length(p) > 1) {
        for(l in slice(p, 1, length(p)-1)) {
            args[[length(args)+1]] <- l
        }
    }
    args <- append(args, p[[length(p)]])
    fapply(f, args)
}

map <- function(f, seq) {
    new.listl(lapply(seq, function(el) fapply(f, el)))
}

core_ns <- list(
    "="=function(a,b) .equal_q(a,b),
    "throw"=function(err) throw(err),
    "nil?"=.nil_q,
    "true?"=.true_q,
    "false?"=.false_q,
    "symbol?"=.symbol_q,
    "symbol"=new.symbol,
    "symbol?"=.symbol_q,

    "pr-str"=pr_str,
    "str"=str,
    "prn"=prn,
    "println"=println,
    "readline"=readline,
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

    "list"=new.list,
    "list?"=function(a) .list_q(a),
    "vector"=new.vector,
    "vector?"=function(a) .vector_q(a),
    "empty?"=function(a) .sequential_q(a) && length(a) == 0,
    "count"=function(a) length(a),

    "sequential?"=.sequential_q,
    "cons"=cons,
    "concat"=do_concat,
    "nth"=function(a,b) if (length(a) < b+1) nil else a[[b+1]],
    "first"=function(a) if (length(a) < 1) nil else a[[1]], 
    "rest"=function(a) new.listl(slice(a,2)),
    "apply"=do_apply,
    "map"=map
)
