..types.. <- TRUE

# General type related functions
concat <- function(...) {
    paste(..., collapse="", sep="")
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

