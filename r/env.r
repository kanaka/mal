..env.. <- TRUE

if(!exists("..types..")) source("types.r")

new.Env <- function(outer=emptyenv(), binds=list(), exprs=list()) {
    e <- structure(new.env(parent=outer), class="Env")

    if (length(binds) > 0) {
        for(i in seq(length(binds))) {
            b <- as.character(binds[[i]])
            if (b == "&") {
                e[[as.character(binds[[i+1]])]] <-
                    slice(exprs, i, length(exprs))
                break
            } else {
                e[[b]] <- exprs[[i]]
            }
        }
    }
    e
}

Env.find <- function(e, key) {
    key <- as.character(key)
    if (exists(key, envir=e, inherits=FALSE)) {
        e
    } else if (!identical(parent.env(e), emptyenv())) {
        Env.find(parent.env(e), key)
    } else {
        nil
    }
}

Env.set <- function(e, key, val) {
    key <- as.character(key)
    e[[key]] <- val
    invisible(val)
}

Env.get <- function(e, key) {
    key <- as.character(key)
    e <- Env.find(e, key)
    if (.nil_q(e)) throw(concat("'", key, "' not found"))
    e[[key]]
}
