..readline.. <- TRUE

HISTORY_FILE = paste(path.expand("~"), "/.mal-history", sep="")

library(rdyncall, lib.loc="lib/")

#.rllib <- dynfind(c("edit"))
.rllib <- dynfind(c("readline"))
.call_readline <- .dynsym(.rllib,"readline")
.call_add_history <- .dynsym(.rllib,"add_history")

.state <- new.env()
.state$rl_history_loaded = FALSE

.readline <- function(prompt) {
    res <- .dyncall(.call_readline, "Z)p", prompt)
    if (is.nullptr(res)) {
        return(NULL)
    } else {
        return(ptr2str(res))
    }
}

readline <- function(prompt) {
    if (!.state$rl_history_loaded) {
        .state$rl_history_loaded <- TRUE

        lines <- scan(HISTORY_FILE, what="", sep="\n", quiet=TRUE)
        for(add_line in lines) {
            .dyncall(.call_add_history, "Z)v", add_line)
        }
    }

    line <- .readline(prompt)
    if (is.null(line)) return(NULL)
    .dyncall(.call_add_history, "Z)v", line)
    write(line, file=HISTORY_FILE, append=TRUE)

    line
}
