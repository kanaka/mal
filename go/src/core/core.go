package core

import (
    "errors"
    "io/ioutil"
    "fmt"
)

import (
    . "types"
    "reader"
    "printer"
)


// String functions

func pr_str(a []MalType) (MalType, error) {
    return printer.Pr_list(a, true, "", "", " "), nil
}

func str(a []MalType) (MalType, error) {
    return printer.Pr_list(a, false, "", "", ""), nil
}

func prn(a []MalType) (MalType, error) {
    fmt.Println(printer.Pr_list(a, true, "", "", " "))
    return nil, nil
}

func println(a []MalType) (MalType, error) {
    fmt.Println(printer.Pr_list(a, false, "", "", " "))
    return nil, nil
}

func slurp(a []MalType) (MalType, error) {
    b, e := ioutil.ReadFile(a[0].(string))
    if e != nil { return nil, e }
    return string(b), nil
}


// Sequence functions

func empty_Q(a []MalType) (MalType, error) {
    switch obj := a[0].(type) {
    case List:   return len(obj.Val) == 0, nil
    case Vector: return len(obj.Val) == 0, nil
    case nil:    return true, nil
    default: return nil, errors.New("Count called on non-sequence")
    }
}

func count(a []MalType) (MalType, error) {
    switch obj := a[0].(type) {
    case List:   return len(obj.Val), nil
    case Vector: return len(obj.Val), nil
    case nil:    return 0, nil
    default: return nil, errors.New("Count called on non-sequence")
    }
}


// core namespace
var NS = map[string]MalType{
    "=": func(a []MalType) (MalType, error) {
            return Equal_Q(a[0], a[1]), nil },

    "pr-str": func(a []MalType) (MalType, error) { return pr_str(a) },
    "str": func(a []MalType) (MalType, error) { return str(a) },
    "prn": func(a []MalType) (MalType, error) { return prn(a) },
    "println": func(a []MalType) (MalType, error) { return println(a) },
    "read-string":  func(a []MalType) (MalType, error) {
            return reader.Read_str(a[0].(string)) },
    "slurp": slurp,

    "<": func(a []MalType) (MalType, error) {
            return a[0].(int) < a[1].(int), nil },
    "<=": func(a []MalType) (MalType, error) {
            return a[0].(int) <= a[1].(int), nil },
    ">": func(a []MalType) (MalType, error) {
            return a[0].(int) > a[1].(int), nil },
    ">=": func(a []MalType) (MalType, error) {
            return a[0].(int) >= a[1].(int), nil },
    "+": func(a []MalType) (MalType, error) {
            return a[0].(int) + a[1].(int), nil },
    "-": func(a []MalType) (MalType, error) {
            return a[0].(int) - a[1].(int), nil },
    "*": func(a []MalType) (MalType, error) {
            return a[0].(int) * a[1].(int), nil },
    "/": func(a []MalType) (MalType, error) {
            return a[0].(int) / a[1].(int), nil },

    "list": func(a []MalType) (MalType, error) {
            return List{a}, nil },
    "list?": func(a []MalType) (MalType, error) {
            return List_Q(a[0]), nil },

    "empty?": empty_Q,
    "count": count,
    }
