package main

import (
    "bufio"
    //"io"
    "fmt"
    "os"
    "strings"
)

import (
    . "types"
    "reader"
    "printer"
)

// read
func READ(str string) (MalType, error) {
    return reader.Read_str(str)
}

// eval
func EVAL(ast MalType, env string) (MalType, error) {
    return ast, nil
}

// print
func PRINT(exp MalType) (string, error) {
    return printer.Pr_str(exp, true), nil
}

// repl
func rep(str string) (MalType, error) {
    var exp MalType
    var res string
    var e error
    if exp, e = READ(str); e != nil { return nil, e }
    if exp, e = EVAL(exp, ""); e != nil { return nil, e }
    if res, e = PRINT(exp); e != nil { return nil, e }
    return res, nil
}

func main() {
    rdr := bufio.NewReader(os.Stdin);
    // repl loop
    for {
        fmt.Print("user> ");
        text, err := rdr.ReadString('\n');
        text = strings.TrimRight(text, "\n");
        if (err != nil) {
            return
        }
        var out MalType
        var e error
        if out, e = rep(text); e != nil {
            if e.Error() == "<empty line>" { continue }
            fmt.Printf("Error: %v\n", e)
            continue
        }
        fmt.Printf("%v\n", out)
    }
}
