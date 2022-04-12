if(!exists("..readline..")) source("readline.r")
if(!exists("..types..")) source("types.r")
if(!exists("..reader..")) source("reader.r")
if(!exists("..printer..")) source("printer.r")
if(!exists("..env..")) source("env.r")
if(!exists("..core..")) source("core.r")

# read
READ <- function(str) {
    return(read_str(str))
}

# eval
starts_with <- function(ast, sym) {
    .list_q(ast) && length(ast) == 2 && .symbol_q(ast[[1]]) && ast[[1]] == sym
}

quasiquote_elements <- function(ast) {
    acc <- new.list()
    i <- length(ast)
    while (0 < i) {
        elt <- ast[[i]]
        if (starts_with(elt, "splice-unquote")) {
            acc = new.list(new.symbol("concat"), elt[[2]], acc)
        } else {
            acc = new.list(new.symbol("cons"), quasiquote(elt), acc)
        }
        i <- i-1
    }
    acc
}

quasiquote <- function(ast) {
    if (.list_q(ast)) {
        if (starts_with(ast, "unquote")) {
            ast[[2]]
        } else {
            quasiquote_elements(ast)
        }
    } else if (.vector_q(ast)) {
        new.list(new.symbol("vec"), quasiquote_elements(ast))
    } else if (.symbol_q(ast) || .hash_map_q(ast)) {
        new.list(new.symbol("quote"), ast)
    } else {
        ast
    }
}

EVAL <- function(ast, env) {

    repeat {

    dbgevalenv <- Env.find(env, "DEBUG-EVAL")
    if (!.nil_q(dbgevalenv)) {
        dbgeval <- Env.get(dbgevalenv, "DEBUG-EVAL")
        if (!.nil_q(dbgeval) && !identical(dbgeval, FALSE))
            cat("EVAL: ", .pr_str(ast,TRUE), "\n", sep="")
    }

    if (.symbol_q(ast)) {
        return(Env.get(env, ast))
    } else if (.list_q(ast)) {
        # exit this switch
    } else if (.vector_q(ast)) {
        return(new.vectorl(lapply(ast, function(a) EVAL(a, env))))
    } else if (.hash_map_q(ast)) {
        lst <- list()
        for(k in ls(ast)) {
            lst[[length(lst)+1]] = k
            lst[[length(lst)+1]] = EVAL(ast[[k]], env)
        }
        return(new.hash_mapl(lst))
    } else {
        return(ast)
    }

    if (length(ast) == 0) { return(ast) }

    # apply list
    switch(paste("l",length(ast),sep=""),
           l0={ return(ast) },
           l1={ a0 <- ast[[1]]; a1 <- NULL;     a2 <- NULL },
           l2={ a0 <- ast[[1]]; a1 <- ast[[2]]; a2 <- NULL },
              { a0 <- ast[[1]]; a1 <- ast[[2]]; a2 <- ast[[3]] })
    if (length(a0) > 1) a0sym <- "__<*fn*>__"
    else                a0sym <- as.character(a0)
    if (a0sym == "def!") {
        res <- EVAL(a2, env)
        return(Env.set(env, a1, res))
    } else if (a0sym == "let*") {
        let_env <- new.Env(env)
        for(i in seq(1,length(a1),2)) {
            Env.set(let_env, a1[[i]], EVAL(a1[[i+1]], let_env))
        }
        ast <- a2
        env <- let_env
    } else if (a0sym == "quote") {
        return(a1)
    } else if (a0sym == "quasiquote") {
        ast <- quasiquote(a1)
    } else if (a0sym == "defmacro!") {
        func <- EVAL(a2, env)
        func$ismacro = TRUE
        return(Env.set(env, a1, func))
    } else if (a0sym == "do") {
        if (2 < length(ast))
            for(i in seq(2, length(ast) - 1))
                EVAL(ast[[i]], env)
        ast <- ast[[length(ast)]]
    } else if (a0sym == "if") {
        cond <- EVAL(a1, env)
        if (.nil_q(cond) || identical(cond, FALSE)) {
            if (length(ast) < 4) return(nil)
            ast <- ast[[4]]
        } else {
            ast <- a2
        }
    } else if (a0sym == "fn*") {
        return(malfunc(EVAL, a2, env, a1))
    } else {
        f <- EVAL(a0, env)
        if (.macro_q(f)) {
            ast <- fapply(f, slice(ast, 2))
            next
        }
        args <- new.listl(lapply(slice(ast, 2), function(a) EVAL(a, env)))
        if (class(f) == "MalFunc") {
            ast <- f$ast
            env <- f$gen_env(args)
        } else {
            return(do.call(f, args))
        }
    }

    }
}

# print
PRINT <- function(exp) {
    return(.pr_str(exp, TRUE))
}

# repl loop
repl_env <- new.Env()
rep <- function(str) return(PRINT(EVAL(READ(str), repl_env)))

# core.r: defined using R
for(k in names(core_ns)) { Env.set(repl_env, k, core_ns[[k]]) }
Env.set(repl_env, "eval", function(ast) EVAL(ast, repl_env))
Env.set(repl_env, "*ARGV*", new.list())

# core.mal: defined using the language itself
. <- rep("(def! not (fn* (a) (if a false true)))")
. <- rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")
. <- rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")


args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
    Env.set(repl_env, "*ARGV*", new.listl(slice(as.list(args),2)))
    . <- rep(concat("(load-file \"", args[[1]], "\")"))
    quit(save="no", status=0)
}

repeat {
    line <- readline("user> ")
    if (is.null(line)) { cat("\n"); break }
    tryCatch({
        cat(rep(line),"\n", sep="")
    }, error=function(err) {
        cat("Error: ", get_error(err),"\n", sep="")
    })
    # R debug/fatal with tracebacks:
    #cat(rep(line),"\n", sep="")
}
