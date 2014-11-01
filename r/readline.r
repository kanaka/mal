..readline.. <- TRUE

library(rdyncall, lib.loc="lib/")

#rllib <- dynfind(c("edit"))
rllib <- dynfind(c("readline"))
rl <- .dynsym(rllib,"readline")

readline <- function(prompt) {
    res <- .dyncall(rl, "Z)p", "user> ")
    if (is.nullptr(res)) {
        return(NULL)
    } else {
        return(ptr2str(res))
    }
}
