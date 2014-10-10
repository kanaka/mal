package types

import (
    "reflect"
    "errors"
    "fmt"
)

// Errors/Exceptions
type MalError struct {
    Obj MalType
}

func (e MalError) Error() string {
    return fmt.Sprintf("%#v", e.Obj)
}


// General types
type MalType interface {
}

type EnvType interface {
    //Find(key string) *EnvType
    Find(key string) EnvType
    Set(key string, value MalType) MalType
    Get(key string) (MalType, error)
}

// Scalars
func Nil_Q(obj MalType) bool {
    switch obj.(type) {
    case nil: return true
    default:  return false
    }
}

func True_Q(obj MalType) bool {
    switch tobj := obj.(type) {
    case bool: return tobj == true
    default:   return false
    }
}

func False_Q(obj MalType) bool {
    switch tobj := obj.(type) {
    case bool: return tobj == false
    default:   return false
    }
}

// Symbols
type Symbol struct {
    Val string
}

func Symbol_Q(obj MalType) bool {
    switch obj.(type) {
    case Symbol: return true
    default:     return false
    }
}


// Strings
func String_Q(obj MalType) bool {
    switch obj.(type) {
    case string: return true
    default:     return false
    }
}


// Functions
type MalFunc struct {
    Eval    func(MalType, EnvType) (MalType, error)
    Exp     MalType
    Env     EnvType
    Params  MalType
    IsMacro bool
    GenEnv  func(EnvType, MalType, MalType) (EnvType, error)
}

func MalFunc_Q(obj MalType) bool {
    switch obj.(type) {
    case MalFunc: return true
    default:      return false
    }
}

func (f MalFunc) SetMacro() MalType {
    f.IsMacro = true
    return f
}

func (f MalFunc) GetMacro() bool {
    return f.IsMacro
}

// Take either a MalFunc or regular function and apply it to the
// arguments
func Apply(f_mt MalType, a []MalType) (MalType, error) {
    switch f := f_mt.(type) {
    case MalFunc:
        env, e := f.GenEnv(f.Env, f.Params, List{a})
        if e != nil { return nil, e }
        return f.Eval(f.Exp, env)
    case func([]MalType)(MalType, error):
        return f(a)
    default:
        return nil, errors.New("Invalid function to Apply")
    }
}


// Lists
type List struct {
    Val []MalType
}

func NewList(a ...MalType) MalType {
    return List{a}
}

func List_Q(obj MalType) bool {
    switch obj.(type) {
    case List: return true
    default:   return false
    }
}

// Vectors
type Vector struct {
    Val []MalType
}

func Vector_Q(obj MalType) bool {
    switch obj.(type) {
    case Vector: return true
    default:     return false
    }
}

func GetSlice(seq MalType) ([]MalType, error) {
    switch obj := seq.(type) {
    case List:   return obj.Val, nil
    case Vector: return obj.Val, nil
    default: return nil, errors.New("GetSlice called on non-sequence")
    }
}

// Hash Maps
func NewHashMap(seq MalType) (MalType, error) {
    lst, e := GetSlice(seq)
    if e != nil { return nil, e }
    if len(lst) % 2 == 1 {
        return nil, errors.New("Odd number of arguments to NewHashMap")
    }
    m := map[string]MalType{}
    for i := 0; i < len(lst); i+=2 {
        str, ok := lst[i].(string)
        if !ok {
            return nil, errors.New("expected hash-map key string")
        }
        m[str] = lst[i+1]
    }
    return m, nil
}

func HashMap_Q(obj MalType) bool {
    switch obj.(type) {
    case map[string]MalType: return true
    default:                 return false
    }
}

// General functions

func _obj_type(obj MalType) string {
    return reflect.TypeOf(obj).Name()
}

func Sequential_Q(seq MalType) bool {
    if seq == nil { return false }
    return (reflect.TypeOf(seq).Name() == "List") ||
           (reflect.TypeOf(seq).Name() == "Vector")
}

func Equal_Q(a MalType, b MalType) bool {
    ota := reflect.TypeOf(a); otb := reflect.TypeOf(b)
    if !((ota == otb) || (Sequential_Q(a) && Sequential_Q(b))) {
        return false
    }
    //av := reflect.ValueOf(a); bv := reflect.ValueOf(b)
    //fmt.Printf("here2: %#v\n", reflect.TypeOf(a).Name())
    switch reflect.TypeOf(a).Name() {
    case "Symbol":
        return a.(Symbol).Val == b.(Symbol).Val
    case "List":   fallthrough
    case "Vector":
        as,_ := GetSlice(a); bs,_ := GetSlice(b)
        if len(as) != len(bs) { return false }
        for i := 0; i < len(as); i+=1 {
            if !Equal_Q(as[i], bs[i]) { return false }
        }
        return true
    case "map[string]MalType":
        return false
    default:
        return a == b
    }
}
