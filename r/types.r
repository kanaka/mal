..types.. <- TRUE

if(!exists("..env..")) source("env.r")

# General type related functions
concat <- function(..., sep="")  paste(..., collapse="", sep=sep)
concatl <- function(lst, sep="") paste(lst, collapse=sep, sep=sep)

slice <- function(seq, start=1, end=-1) {
    if (end == -1) end <- length(seq)
    if (start > length(seq)) lst <- list() else lst <- seq[start:end]
    switch(class(seq),
        list={ new.listl(lst) },
        List={ new.listl(lst) },
        Vector={ new.vectorl(lst) },
        { throw("slice called on non-sequence") })
}

.sequential_q <- function(obj) .list_q(obj) || .vector_q(obj)

.equal_q <- function(a,b) {
    ota <- class(a); otb <- class(b)
    if (!((ota == otb) || (.sequential_q(a) && .sequential_q(b)))) {
        return(FALSE)
    }
    switch(ota,
    "List"={
        if (length(a) != length(b)) return(FALSE)
        if (length(a) == 0) return(TRUE)
        for(i in seq(length(a))) {
            if (!.equal_q(a[[i]],b[[i]])) return(FALSE)
        }
        TRUE
    },
    "Vector"={
        if (length(a) != length(b)) return(FALSE)
        if (length(a) == 0) return(TRUE)
        for(i in seq(length(a))) {
            if (!.equal_q(a[[i]],b[[i]])) return(FALSE)
        }
        TRUE
    },
    {
        a == b
    })
}

# Errors/exceptions
thrown_error = new.env()
thrown_error$val = NULL
throw <- function(obj) {
    thrown_error$val = obj
    stop("<mal_exception>")
}
get_error <- function(e) {
    estr <- e$message
    if (estr == "<mal_exception>") {
        err <- thrown_error$val
        thrown_error$val <- NULL
        err
    } else {
        estr
    }
}

# Scalars
nil <- structure("malnil", class="nil")
.nil_q <- function(obj) "nil" == class(obj)
.true_q <- function(obj) "logical" == class(obj) && obj == TRUE
.false_q <- function(obj) "logical" == class(obj) && obj == FALSE
new.symbol <- function(name) structure(name, class="Symbol")
.symbol_q <- function(obj) "Symbol" == class(obj)

# Functions

malfunc <- function(eval, ast, env, params) {
    gen_env <- function(args) new.Env(env, params, args)
    structure(list(eval=eval,
                   ast=ast,
                   env=env,
                   params=params,
                   gen_env=gen_env,
                   ismacro=TRUE), class="MalFunc")
}
.malfunc_q <- function(obj) "MalFunc" == class(obj)

fapply <- function(mf, args) {
    if (class(mf) == "MalFunc") {
        ast <- mf$ast
        env <- mf$gen_env(args)
        mf$eval(ast, env)
    } else {
        #print(args)
        do.call(mf,args)
    }
}

# Lists
new.list <- function(...) {
    lst <- list(...)
    class(lst) <- "List"
    lst
}
new.listl <- function(lst) {
    class(lst) <- "List"
    lst
}
.list_q <- function(obj) "List" == class(obj)

# Vectors
new.vector <- function(...) {
    lst <- list(...)
    class(lst) <- "Vector"
    lst
}
new.vectorl <- function(lst) {
    class(lst) <- "Vector"
    lst
}
.vector_q <- function(obj) "Vector" == class(obj)

