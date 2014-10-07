package types

import (
    "reflect"
    "errors"
    //"fmt"
)

//import (
//    "env"
//)

type MalType interface {
}

type EnvType interface {
    //Find(key string) *EnvType
    Find(key string) EnvType
    Set(key string, value MalType) MalType
    Get(key string) (MalType, error)
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


// Functions
type MalFunc struct {
    Eval    func(MalType, EnvType) (MalType, error)
    Exp     MalType
    Env     EnvType
    Params  MalType
    IsMacro bool
    GenEnv  func(EnvType, []MalType, []MalType) (EnvType, error)
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

func (f MalFunc) Apply(a []MalType) (MalType, error) {
    slc, e := GetSlice(f.Params)
    if e != nil { return nil, e }
    env, e := f.GenEnv(f.Env, slc, a)
    if e != nil { return nil, e }
    return f.Eval(f.Exp, env)
}


// Lists
type List struct {
    Val []MalType
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
func Hash_Map_Q(obj MalType) bool {
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
    //fmt.Printf("here1 %#v\n", reflect.TypeOf(seq).Name())
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
