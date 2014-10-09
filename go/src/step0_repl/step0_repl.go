package main

import (
    "fmt"
    "strings"
)

import (
    "readline"
)

// read
func READ(str string) string {
    return str
}

// eval
func EVAL(ast string, env string) string {
    return ast
}

// print
func PRINT(exp string) string {
    return exp
}

// repl
func rep(str string) string {
    return PRINT(EVAL(READ(str), ""))
}

func main() {
    // repl loop
    for {
        text, err := readline.Readline("user> ")
        text = strings.TrimRight(text, "\n");
        if (err != nil) {
            return
        }
        fmt.Println(rep(text))
    }
}
