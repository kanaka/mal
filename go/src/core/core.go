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
    "readline"
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


// Hash Map functions
func assoc(a []MalType) (MalType, error) {
    if len(a) <3 { return nil, errors.New("assoc requires at least 3 arguments") }
    if (len(a) % 2 != 1) { return nil, errors.New("assoc requires odd number of arguments") }
    if !HashMap_Q(a[0]) { return nil, errors.New("assoc called on non-hash map") }
    old_hm := a[0].(map[string]MalType)
    new_hm := map[string]MalType{}
    // copy
    for k, v := range old_hm { new_hm[k] = v }
    for i := 1; i < len(a); i+=2 {
        key := a[i]
        if !String_Q(key) { return nil, errors.New("assoc called with non-string key") }
        new_hm[key.(string)] = a[i+1]
    }
    return new_hm, nil
}

func dissoc(a []MalType) (MalType, error) {
    if len(a) <2 { return nil, errors.New("dissoc requires at least 3 arguments") }
    if !HashMap_Q(a[0]) { return nil, errors.New("dissoc called on non-hash map") }
    old_hm := a[0].(map[string]MalType)
    new_hm := map[string]MalType{}
    // copy
    for k, v := range old_hm { new_hm[k] = v }
    for i := 1; i < len(a); i+=1 {
        key := a[i]
        if !String_Q(key) { return nil, errors.New("dissoc called with non-string key") }
        delete(new_hm,key.(string))
    }
    return new_hm, nil
}

func get(a []MalType) (MalType, error) {
    if len(a) != 2 { return nil, errors.New("get requires 2 arguments") }
    if Nil_Q(a[0]) { return nil, nil }
    if !HashMap_Q(a[0]) { return nil, errors.New("get called on non-hash map") }
    if !String_Q(a[1]) { return nil, errors.New("get called with non-string key") }
    return a[0].(map[string]MalType)[a[1].(string)], nil
}

func contains_Q(hm MalType, key MalType) (MalType, error) {
    if Nil_Q(hm) { return nil, nil }
    if !HashMap_Q(hm) { return nil, errors.New("get called on non-hash map") }
    if !String_Q(key) { return nil, errors.New("get called with non-string key") }
    _, ok := hm.(map[string]MalType)[key.(string)]
    return ok, nil
}

func keys(a []MalType) (MalType, error) {
    if !HashMap_Q(a[0]) { return nil, errors.New("keys called on non-hash map") }
    slc := []MalType{}
    for k, _ := range a[0].(map[string]MalType) {
        slc = append(slc, k)
    }
    return List{slc}, nil
}
func vals(a []MalType) (MalType, error) {
    if !HashMap_Q(a[0]) { return nil, errors.New("keys called on non-hash map") }
    slc := []MalType{}
    for _, v := range a[0].(map[string]MalType) {
        slc = append(slc, v)
    }
    return List{slc}, nil
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
    case map[string]MalType: return len(obj), nil
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
    "readline": func(a []MalType) (MalType, error) {
            return readline.Readline(a[0].(string)) },

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
    "vector": func(a []MalType) (MalType, error) {
            return Vector{a}, nil },
    "vector?": func(a []MalType) (MalType, error) {
            return Vector_Q(a[0]), nil },
    "hash-map": func(a []MalType) (MalType, error) {
            return NewHashMap(List{a}) },
    "map?": func(a []MalType) (MalType, error) {
            return HashMap_Q(a[0]), nil },
    "assoc": assoc,
    "dissoc": dissoc,
    "get": get,
    "contains?": func(a []MalType) (MalType, error) {
            return contains_Q(a[0], a[1]) },
    "keys": keys,
    "vals": vals,

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
