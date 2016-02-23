
enum MalError: ErrorType {
    case Reader(msg: String)
    case General(msg: String)
}

enum MalVal {
    case MalNil
    case MalTrue
    case MalFalse
    case MalInt(Int)
    case MalFloat(Float)
    case MalString(String)
    case MalSymbol(String)
    case MalList(Array<MalVal>)
    case MalVector(Array<MalVal>)
    // TODO: ast and params wrapped in arrays because otherwise
    // compiler throws a fault
    case MalFunc((Array<MalVal>) throws -> MalVal,
                 ast: Array<MalVal>?, env: Env?, params: Array<MalVal>?)
}

typealias MV = MalVal

// General functions

func wraptf(a: Bool) -> MalVal {
    return a ? MV.MalTrue : MV.MalFalse
}

func cmp_seqs(a: Array<MalVal>, _ b: Array<MalVal>) -> Bool {
    if a.count != b.count { return false }
    var idx = a.startIndex
    while idx < a.endIndex {
        if !equal_Q(a[idx], b[idx]) { return false }
        idx = idx.successor()
    }
    return true
}

func equal_Q(a: MalVal, _ b: MalVal) -> Bool {
    switch (a, b) {
    case (MV.MalNil, MV.MalNil): return true
    case (MV.MalFalse, MV.MalFalse): return true
    case (MV.MalTrue, MV.MalTrue): return true
    case (MV.MalInt(let i1), MV.MalInt(let i2)): return i1 == i2
    case (MV.MalString(let s1), MV.MalString(let s2)): return s1 == s2
    case (MV.MalSymbol(let s1), MV.MalSymbol(let s2)): return s1 == s2
    case (MV.MalList(let l1), MV.MalList(let l2)):
        return cmp_seqs(l1, l2)
    case (MV.MalList(let l1), MV.MalVector(let l2)):
        return cmp_seqs(l1, l2)
    case (MV.MalVector(let l1), MV.MalList(let l2)):
        return cmp_seqs(l1, l2)
    case (MV.MalVector(let l1), MV.MalVector(let l2)):
        return cmp_seqs(l1, l2)
    default:
        return false
    }
}

func rest(a: MalVal) throws -> MalVal {
    switch a {
    case MV.MalList(let lst):
        let slc = lst[lst.startIndex.successor()..<lst.endIndex]
        return MV.MalList(Array(slc))
    default:
        throw MalError.General(msg: "Invalid rest call")
    }
}
