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

// Errors/Exceptions
func throw(a []MalType) (MalType, error) {
    return nil, MalError{a[0]}
}


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

func cons(a []MalType) (MalType, error) {
    val := a[0]
    lst, e := GetSlice(a[1]); if e != nil { return nil, e }

    return List{append([]MalType{val}, lst...)}, nil
}

func concat(a []MalType) (MalType, error) {
    if len(a) == 0 { return List{}, nil }
    slc1, e := GetSlice(a[0]); if e != nil { return nil, e }
    for i := 1; i < len(a); i+=1 {
        slc2, e := GetSlice(a[i]); if e != nil { return nil, e }
        slc1 = append(slc1, slc2...)
    }
    return List{slc1}, nil
}

func nth(a []MalType) (MalType, error) {
    slc, e := GetSlice(a[0]); if e != nil { return nil, e }
    idx := a[1].(int)
    return slc[idx], nil
}

func first(a []MalType) (MalType, error) {
    if len(a) == 0 { return nil, nil }
    slc, e := GetSlice(a[0]); if e != nil { return nil, e }
    if len(slc) == 0 { return nil, nil }
    return slc[0], nil
}

func rest(a []MalType) (MalType, error) {
    slc, e := GetSlice(a[0]); if e != nil { return nil, e }
    if len(slc) == 0 { return List{}, nil }
    return List{slc[1:]}, nil
}


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

func apply(a []MalType) (MalType, error) {
    if len(a) < 2 { return nil, errors.New("apply requires at least 2 args") }
    f := a[0]
    args := []MalType{}
    for _, b := range a[1:len(a)-1] {
        args = append(args, b)
    }
    last, e := GetSlice(a[len(a)-1]); if e != nil { return nil, e }
    args = append(args, last...)
    return Apply(f, args)
}

func do_map(a []MalType) (MalType, error) {
    if len(a) != 2 { return nil, errors.New("map requires 2 args") }
    f := a[0]
    results := []MalType{}
    args, e := GetSlice(a[1]); if e != nil { return nil, e }
    for _, arg := range args {
        res, e := Apply(f, []MalType{arg})
        results = append(results, res)
        if e != nil { return nil, e }
    }
    return List{results}, nil
}



// core namespace
var NS = map[string]MalType{
    "=": func(a []MalType) (MalType, error) {
            return Equal_Q(a[0], a[1]), nil },
    "throw": throw,
    "nil?": func(a []MalType) (MalType, error) {
            return Nil_Q(a[0]), nil },
    "true?": func(a []MalType) (MalType, error) {
            return True_Q(a[0]), nil },
    "false?": func(a []MalType) (MalType, error) {
            return False_Q(a[0]), nil },
    "symbol?":  func(a []MalType) (MalType, error) {
            return Symbol_Q(a[0]), nil },

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

    "sequential?":  func(a []MalType) (MalType, error) {
            return Sequential_Q(a[0]), nil },
    "cons": cons,
    "concat": concat,
    "nth": nth,
    "first": first,
    "rest": rest,
    "empty?": empty_Q,
    "count": count,
    "apply": apply,
    "map": do_map,
    }
