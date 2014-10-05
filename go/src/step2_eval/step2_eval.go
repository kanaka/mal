package main

import (
    "bufio"
    //"io"
    "fmt"
    "os"
    "strings"
    "errors"
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
func eval_ast(ast MalType, env map[string]MalType) (MalType, error) {
    //fmt.Printf("eval_ast: %#v\n", ast)
    if Symbol_Q(ast) {
        k := ast.(Symbol).Val
        exp, ok := env[k]
        if !ok { return nil, errors.New("'" + k + "' not found") }
        return exp, nil
    } else if List_Q(ast) {
        lst := []MalType{}
        for _, a := range ast.(List).Val {
            exp, e := EVAL(a, env)
            if e != nil { return nil, e }
            lst = append(lst, exp)
        }
        return List{lst}, nil
    } else {
        return ast, nil
    }
}

func EVAL(ast MalType, env map[string]MalType) (MalType, error) {
    //fmt.Printf("EVAL: %#v\n", ast)
    switch ast.(type) {
    case List: // continue
    default:   return eval_ast(ast, env)
    }

    el, e := eval_ast(ast, env)
    if e != nil { return nil, e }
    f, ok := el.(List).Val[0].(func([]MalType)(MalType, error))
    if !ok { return nil, errors.New("attempt to call non-function") }
    return f(el.(List).Val[1:])
}

// print
func PRINT(exp MalType) (MalType, error) {
    return printer.Pr_str(exp, true), nil
}

var repl_env = map[string]MalType{
    "+": func(a []MalType) (MalType, error) {
            return a[0].(int) + a[1].(int), nil
         },
    "-": func(a []MalType) (MalType, error) {
            return a[0].(int) - a[1].(int), nil
         },
    "*": func(a []MalType) (MalType, error) {
            return a[0].(int) * a[1].(int), nil
         },
    "/": func(a []MalType) (MalType, error) {
            return a[0].(int) / a[1].(int), nil
         },
}

// repl
func rep(str string) (MalType, error) {
    var exp MalType
    var e error
    if exp, e = READ(str); e != nil { return nil, e }
    if exp, e = EVAL(exp, repl_env); e != nil { return nil, e }
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
