..reader.. <- TRUE

if(!exists("..types..")) source("types.r")

new.Reader <- function(tokens) {
    e <- structure(new.env(), class="Reader")
    e$tokens <- tokens
    e$position <- 1
    e
}

Reader.peek <- function(rdr) {
    if (rdr$position > length(rdr$tokens)) return(NULL)
    rdr$tokens[[rdr$position]]
}

Reader.next <- function(rdr) {
    if (rdr$position > length(rdr$tokens)) return(NULL)
    rdr$position <- rdr$position + 1
    rdr$tokens[[rdr$position-1]]
}

tokenize <- function(str) {
    re <- "[\\s,]*(~@|[\\[\\]\\{\\}\\(\\)'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;.*|[^\\s\\[\\]\\{\\}\\('\"`,;\\)]*)"
    m <- lapply(regmatches(str, gregexpr(re, str, perl=TRUE)), 
                function(e) sub("^[\\s,]+", "", e, perl=TRUE))
    res <- list()
    i <- 1
    for(v in m[[1]]) {
        if (v == "" || substr(v,1,1) == ";") next
        res[[i]] <- v
        i <- i+1
    }
    res
}

re_match <- function(re, str) { length(grep(re, c(str))) > 0 }

read_atom <- function(rdr) {
    token <- Reader.next(rdr)
    if (re_match("^-?[0-9]+$", token)) {
        as.integer(token)
    } else if (re_match("^-?[0-9][0-9.]*$", token)) {
        as.double(token)
    } else if (substr(token,1,1) == "\"") {
        gsub("\\\\n", "\\n",
             gsub("\\\\\"", "\"",
                  substr(token, 2, nchar(token)-1)))
    } else if (substr(token,1,1) == ":") {
        new.keyword(substring(token,2))
    } else if (token == "nil") {
        nil
    } else if (token == "true") {
        TRUE
    } else if (token == "false") {
        FALSE
    } else {
        new.symbol(token)
    }
}

read_seq <- function(rdr, start="(", end=")") {
    lst <- list()
    token <- Reader.next(rdr)
    if (token != start) {
        throw(concat("expected '", start, "'"))
    }
    repeat {
        token <- Reader.peek(rdr)
        if (is.null(token)) {
            throw(concat("expected '", end, "', got EOF"))
        }
        if (token == end) break
        lst[[length(lst)+1]] <- read_form(rdr)
    }
    Reader.next(rdr)
    new.listl(lst)
}

read_form <- function(rdr) {
    token <- Reader.peek(rdr)
    if (token == "'") {
        . <- Reader.next(rdr);
        new.list(new.symbol("quote"), read_form(rdr))
    } else if (token == "`") {
        . <- Reader.next(rdr);
        new.list(new.symbol("quasiquote"), read_form(rdr))
    } else if (token == "~") {
        . <- Reader.next(rdr);
        new.list(new.symbol("unquote"), read_form(rdr))
    } else if (token == "~@") {
        . <- Reader.next(rdr);
        new.list(new.symbol("splice-unquote"), read_form(rdr))
    } else if (token == "^") {
        . <- Reader.next(rdr)
        m <- read_form(rdr)
        new.list(new.symbol("with-meta"), read_form(rdr), m)
    } else if (token == "@") {
        . <- Reader.next(rdr);
        new.list(new.symbol("deref"), read_form(rdr))
    } else if (token == ")") {
        throw("unexpected ')'")
    } else if (token == "(") {
        new.listl(read_seq(rdr))
    } else if (token == "]") {
        throw("unexpected ']'")
    } else if (token == "[") {
        new.vectorl(read_seq(rdr, "[", "]"))
    } else if (token == "}") {
        throw("unexpected '}'")
    } else if (token == "{") {
        new.hash_mapl(read_seq(rdr, "{", "}"))
    } else {
        read_atom(rdr)
    }
}

read_str <- function(str) {
    tokens <- tokenize(str)
    if (length(tokens) == 0) return(nil)
    return(read_form(new.Reader(tokens)))
}

#cat("---\n")
#print(tokenize("123"))
#cat("---\n")
#print(tokenize("  (  123 456 abc   \"def\"  )  "))

#rdr <- new.reader(tokenize("  (  123 456 abc   \"def\"  )  "))
#Reader.peek(rdr)
#Reader.next(rdr)
#Reader.next(rdr)
#Reader.next(rdr)
#Reader.next(rdr)
#Reader.next(rdr)
#Reader.next(rdr)
#Reader.next(rdr)
