..env.. <- TRUE

if(!exists("..types..")) source("types.r")

new.Env <- function(outer=emptyenv()) {
    structure(new.env(parent=outer), class="Env")
}

Env.find <- function(e, key) {
    key <- as.character(key)
    if (exists(key, envir=e, inherits=FALSE)) {
        e
    } else if (!identical(parent.env(e), emptyenv())) {
        Env.find(parent.env(e), key)
    } else {
        NULL
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
    if (is.null(e)) throw(concat("'", key, "' not found"))
    e[[key]]
}
