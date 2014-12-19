package main

import (
    "fmt"
    "strings"
    "errors"
)

import (
    "readline"
    . "types"
    "reader"
    "printer"
    . "env"
    "core"
)

// read
func READ(str string) (MalType, error) {
    return reader.Read_str(str)
}

// eval
func eval_ast(ast MalType, env EnvType) (MalType, error) {
    //fmt.Printf("eval_ast: %#v\n", ast)
    if Symbol_Q(ast) {
        return env.Get(ast.(Symbol))
    } else if List_Q(ast) {
        lst := []MalType{}
        for _, a := range ast.(List).Val {
            exp, e := EVAL(a, env)
            if e != nil { return nil, e }
            lst = append(lst, exp)
        }
        return List{lst,nil}, nil
    } else if Vector_Q(ast) {
        lst := []MalType{}
        for _, a := range ast.(Vector).Val {
            exp, e := EVAL(a, env)
            if e != nil { return nil, e }
            lst = append(lst, exp)
        }
        return Vector{lst,nil}, nil
    } else if HashMap_Q(ast) {
        m := ast.(HashMap)
        new_hm := HashMap{map[string]MalType{},nil}
        for k, v := range m.Val {
            ke, e1 := EVAL(k, env)
            if e1 != nil { return nil, e1 }
            if _, ok := ke.(string); !ok {
                return nil, errors.New("non string hash-map key")
            }
            kv, e2 := EVAL(v, env)
            if e2 != nil { return nil, e2 }
            new_hm.Val[ke.(string)] = kv
        }
        return new_hm, nil
    } else {
        return ast, nil
    }
}

func EVAL(ast MalType, env EnvType) (MalType, error) {
    for {

    //fmt.Printf("EVAL: %v\n", printer.Pr_str(ast, true))
    switch ast.(type) {
    case List: // continue
    default:   return eval_ast(ast, env)
    }

    // apply list
    a0 := ast.(List).Val[0]
    var a1 MalType = nil; var a2 MalType = nil
    switch len(ast.(List).Val) {
    case 1:
        a1 = nil; a2 = nil
    case 2:
        a1 = ast.(List).Val[1]; a2 = nil
    default:
        a1 = ast.(List).Val[1]; a2 = ast.(List).Val[2]
    }
    a0sym := "__<*fn*>__"
    if Symbol_Q(a0) { a0sym = a0.(Symbol).Val } 
    switch a0sym {
    case "def!":
        res, e := EVAL(a2, env)
        if e != nil { return nil, e }
        return env.Set(a1.(Symbol), res), nil
    case "let*":
        let_env, e := NewEnv(env, nil, nil)
        if e != nil { return nil, e }
        arr1, e := GetSlice(a1)
        if e != nil { return nil, e }
        for i := 0; i < len(arr1); i+=2 {
            if !Symbol_Q(arr1[i]) {
                return nil, errors.New("non-symbol bind value")
            }
            exp, e := EVAL(arr1[i+1], let_env)
            if e != nil { return nil, e }
            let_env.Set(arr1[i].(Symbol), exp)
        }
        ast = a2
        env = let_env
    case "do":
        lst := ast.(List).Val
        _, e := eval_ast(List{lst[1:len(lst)-1],nil}, env) 
        if e != nil { return nil, e }
        if len(lst) == 1 { return nil, nil }
        ast = lst[len(lst)-1]
    case "if":
        cond, e := EVAL(a1, env)
        if e != nil { return nil, e }
        if cond == nil || cond == false {
            if len(ast.(List).Val) >= 4 {
                ast = ast.(List).Val[3]
            } else {
                return nil, nil
            }
        } else {
            ast = a2
        }
    case "fn*":
        fn := MalFunc{EVAL, a2, env, a1, false, NewEnv, nil}
        return fn, nil
    default:
        el, e := eval_ast(ast, env)
        if e != nil { return nil, e }
        f := el.(List).Val[0]
        if MalFunc_Q(f) {
            fn := f.(MalFunc)
            ast = fn.Exp
            env, e = NewEnv(fn.Env, fn.Params, List{el.(List).Val[1:],nil})
            if e != nil { return nil, e }
        } else {
            fn, ok := f.(Func)
            if !ok { return nil, errors.New("attempt to call non-function") }
            return fn.Fn(el.(List).Val[1:])
        }
    }

    } // TCO loop
}

// print
func PRINT(exp MalType) (string, error) {
    return printer.Pr_str(exp, true), nil
}


var repl_env, _ = NewEnv(nil, nil, nil)

// repl
func rep(str string) (MalType, error) {
    var exp MalType
    var res string
    var e error
    if exp, e = READ(str); e != nil { return nil, e }
    if exp, e = EVAL(exp, repl_env); e != nil { return nil, e }
    if res, e = PRINT(exp); e != nil { return nil, e }
    return res, nil
}

func main() {
    // core.go: defined using go
    for k, v := range core.NS {
        repl_env.Set(Symbol{k}, Func{v.(func([]MalType)(MalType,error)),nil})
    }

    // core.mal: defined using the language itself
    rep("(def! not (fn* (a) (if a false true)))")

    // repl loop
    for {
        text, err := readline.Readline("user> ")
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
