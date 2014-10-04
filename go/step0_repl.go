package main

import (
    "bufio"
    //"io"
    "fmt"
    "os"
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
    reader := bufio.NewReader(os.Stdin);
    // repl loop
    for {
        fmt.Print("user> ");
        text, err := reader.ReadString('\n');
        if (err != nil) {
            return
        }
        fmt.Println(rep(text))
    }
}
