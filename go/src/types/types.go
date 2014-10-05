package types

import (
    "errors"
)

//type Error interface {
//    error
//}

type MalType interface {
}

type Symbol struct {
    Val string
}

type List struct {
    Val []MalType
}

type Vector struct {
    Val []MalType
}

// Symbols
func Symbol_Q(obj MalType) bool {
    switch obj.(type) {
    case Symbol: return true
    default:     return false
    }
}

// Lists
func List_Q(obj MalType) bool {
    switch obj.(type) {
    case List: return true
    default:   return false
    }
}

// Vectors
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
