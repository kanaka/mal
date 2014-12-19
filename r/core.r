..core.. <- TRUE

if(!exists("..types..")) source("types.r")
if(!exists("..printer..")) source("printer.r")


# String functions

pr_str <- function(...)
    .pr_list(list(...), print_readably=TRUE, join=" ")

str <- function(...)
    .pr_list(list(...), print_readably=FALSE, join="")

prn <- function(...) {
    cat(.pr_list(list(...), print_readably=TRUE, join=" "))
    cat("\n")
    nil
}

println <- function(...) {
    cat(.pr_list(list(...), print_readably=FALSE, join=" "))
    cat("\n")
    nil
}

do_readline <- function(prompt) {
    l <- readline(prompt)
    if (is.null(l)) nil else l
}

# Hash Map functions
do_get <- function(hm,k) {
    if (class(hm) == "nil") return(nil)
    v <- hm[[k]]
    if (is.null(v)) nil else v
}
contains_q <-function(hm,k) {
    if (class(hm) == "nil") return(FALSE)
    if (is.null(hm[[k]])) FALSE else TRUE
}

# Sequence functions
cons <- function(a,b) {
    new_lst <- append(list(a), b)
    new.listl(new_lst)
}

nth <- function(a,b) {
    if (b < length(a))
        a[[b+1]]
    else
        throw("nth: index out of range")
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
    new.listl(lapply(seq, function(el) fapply(f, list(el))))
}

conj <- function(obj, ...) {
    p <- list(...)
    new_obj <- .clone(obj)
    if (.list_q(obj)) {
        if (length(p) > 0) {
            for(l in p) new_obj <- append(list(l), new_obj)
        }
        new.listl(new_obj)
    } else if (.vector_q(obj)) {
        if (length(p) > 0) {
            for(l in p) new_obj <- append(new_obj, list(l))
        }
        new.vectorl(new_obj)
    } else {
        throw("conj called on non-sequence")
    }
}

# Metadata functions
with_meta <- function(obj, m) {
    new_obj <- .clone(obj)
    attr(new_obj, "meta") <- m
    new_obj
}

meta <- function(obj) {
    m <- attr(obj, "meta")
    if (is.null(m)) nil else m
}

# Atom functions
deref <- function(atm) atm$val
reset_bang <- function (atm, val) { atm$val <- val; val }
swap_bang <- function (atm, f, ...) {
    p <- list(...)
    args <- list(atm$val)
    if (length(p) > 0) {
        for(l in p) args[[length(args)+1]] <- l
    }
    atm$val <- fapply(f, args)
}

core_ns <- list(
    "="=function(a,b) .equal_q(a,b),
    "throw"=function(err) throw(err),
    "nil?"=.nil_q,
    "true?"=.true_q,
    "false?"=.false_q,
    "symbol"=new.symbol,
    "symbol?"=.symbol_q,
    "keyword"=new.keyword,
    "keyword?"=.keyword_q,

    "pr-str"=pr_str,
    "str"=str,
    "prn"=prn,
    "println"=println,
    "readline"=do_readline,
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
    "time-ms"=function() round(as.numeric(Sys.time())*1000),

    "list"=new.list,
    "list?"=function(a) .list_q(a),
    "vector"=new.vector,
    "vector?"=function(a) .vector_q(a),
    "hash-map"=new.hash_map,
    "map?"=function(a) .hash_map_q(a),
    "assoc"=function(hm,...) .assoc(hm,list(...)),
    "dissoc"=function(hm,...) .dissoc(hm,list(...)),
    "get"=do_get,
    "contains?"=contains_q,
    "keys"=function(hm) new.listl(ls(hm)),
    "vals"=function(hm) new.listl(lapply(ls(hm), function(x) hm[[x]])),

    "sequential?"=.sequential_q,
    "cons"=cons,
    "concat"=do_concat,
    "nth"=nth,
    "first"=function(a) if (length(a) < 1) nil else a[[1]], 
    "rest"=function(a) new.listl(slice(a,2)),
    "empty?"=function(a) .sequential_q(a) && length(a) == 0,
    "count"=function(a) if (.nil_q(a)) 0 else length(a),
    "apply"=do_apply,
    "map"=map,
    "conj"=conj,

    "with-meta"=with_meta,
    "meta"=meta,
    "atom"=new.atom,
    "atom?"=.atom_q,
    "deref"=deref,
    "reset!"=reset_bang,
    "swap!"=swap_bang
)
