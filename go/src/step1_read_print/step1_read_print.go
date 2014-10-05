package main

import (
    "bufio"
    //"io"
    "fmt"
    "os"
    "strings"
)

import (
    "types"
    "reader"
)

// read
func READ(str string) (types.MalType, error) {
    return reader.Read_str(str)
}

// eval
func EVAL(ast types.MalType, env string) (types.MalType, error) {
    return ast, nil
}

// print
func PRINT(exp types.MalType) (types.MalType, error) {
    return exp, nil
}

// repl
func rep(str string) (types.MalType, error) {
    var exp types.MalType
    var e error
    if exp, e = READ(str); e != nil { return nil, e }
    if exp, e = EVAL(exp, ""); e != nil { return nil, e }
    if exp, e = PRINT(exp); e != nil { return nil, e }
    return exp, nil
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
        var out types.MalType
        var e error
        if out, e = rep(text); e != nil {
            if e.Error() == "<empty line>" { continue }
            fmt.Printf("Error: %v\n", e)
            continue
        }
        fmt.Printf("%#v\n", out)
    }
}
