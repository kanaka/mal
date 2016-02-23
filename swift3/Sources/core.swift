func IntOp(op: (Int, Int) -> Int, _ a: MalVal, _ b: MalVal) throws -> MalVal {
    switch (a, b) {
    case (MV.MalInt(let i1), MV.MalInt(let i2)):
        return MV.MalInt(op(i1, i2))
    default:
        throw MalError.General(msg: "Invalid IntOp call")
    }
}

func CmpOp(op: (Int, Int) -> Bool, _ a: MalVal, _ b: MalVal) throws -> MalVal {
    switch (a, b) {
    case (MV.MalInt(let i1), MV.MalInt(let i2)):
        return wraptf(op(i1, i2))
    default:
        throw MalError.General(msg: "Invalid CmpOp call")
    }
}



let core_ns: Dictionary<String,(Array<MalVal>) throws -> MalVal> = [
    "=":  { wraptf(equal_Q($0[0], $0[1])) },

    "pr-str":  {
        return MV.MalString($0.map { pr_str($0,true) }.joinWithSeparator(" "))
    },
    "str": {
        return MV.MalString($0.map { pr_str($0,false) }.joinWithSeparator(""))
    },
    "prn": {
        print($0.map { pr_str($0,true) }.joinWithSeparator(" "))
        return MV.MalNil
    },
    "println": {
        print($0.map { pr_str($0,false) }.joinWithSeparator(" "))
        return MV.MalNil
    },


    "<":  { try CmpOp({ $0 < $1},  $0[0], $0[1]) },
    "<=": { try CmpOp({ $0 <= $1}, $0[0], $0[1]) },
    ">":  { try CmpOp({ $0 > $1},  $0[0], $0[1]) },
    ">=": { try CmpOp({ $0 >= $1}, $0[0], $0[1]) },
    "+":  { try IntOp({ $0 + $1},  $0[0], $0[1]) },
    "-":  { try IntOp({ $0 - $1},  $0[0], $0[1]) },
    "*":  { try IntOp({ $0 * $1},  $0[0], $0[1]) },
    "/":  { try IntOp({ $0 / $1},  $0[0], $0[1]) },

    "list": { MV.MalList($0) },
    "list?": {
        switch $0[0] {
        case MV.MalList(_): return MV.MalTrue
        default: return MV.MalFalse
        }
    },

    "empty?": {
        switch $0[0] {
        case MV.MalList(let lst):
            return lst.count == 0 ? MV.MalTrue : MV.MalFalse
        case MV.MalNil: return MV.MalTrue
        default: throw MalError.General(msg: "Invalid empty? call")
        }
    },
    "count": {
        switch $0[0] {
        case MV.MalList(let lst): return MV.MalInt(lst.count)
        case MV.MalNil: return MV.MalInt(0)
        default: throw MalError.General(msg: "Invalid count call")
        }
    }
]
