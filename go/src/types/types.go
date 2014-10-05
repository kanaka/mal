package types

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

// Hash Maps
func Hash_Map_Q(obj MalType) bool {
    switch obj.(type) {
    case map[string]MalType: return true
    default:                 return false
    }
}
