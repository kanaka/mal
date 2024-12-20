class Env {
    var outer: Env? = nil
    var data: Dictionary<String, MalVal> = [:]

    init(_ outer: Env? = nil, binds: MalVal? = nil,
                              exprs: MalVal? = nil) throws {
        self.outer = outer

        if binds != nil {
            var bs = Array<MalVal>(), es = Array<MalVal>()
            //print("binds: \(binds), exprs: \(exprs)")
            switch (binds!, exprs!) {
            case (MalVal.MalList(let l1, _), MalVal.MalList(let l2, _)):
                bs = l1; es = l2
            case (MalVal.MalVector(let l1, _), MalVal.MalList(let l2, _)):
                bs = l1; es = l2
            default:
                throw MalError.General(msg: "invalid Env init call")
            }

            var pos = bs.startIndex

            bhandle:
            while pos < bs.endIndex {
                let b = bs[pos]
                switch b {
                case MalVal.MalSymbol("&"):
                    switch bs[bs.index(after: pos)] {
                    case MalVal.MalSymbol(let sym):
                        if pos < es.endIndex {
                            let slc = es[pos..<es.endIndex]
                            data[sym] = list(Array(slc))
                        } else {
                            data[sym] = list([])
                        }
                        break bhandle
                    default:
                        throw MalError.General(msg: "Env invalid varargs")
                    }
                case MalVal.MalSymbol(let sym):
                    let e = es[pos]
                    data[sym] = e
                default:
                    throw MalError.General(msg: "Env binds has non-symbol")
                }
                pos = bs.index(after: pos)
            }
        }
    }

    func get(_ str: String) -> MalVal? {
        return data[str] ?? outer?.get(str)
    }

    @discardableResult
    func set(_ key: MalVal, _ val: MalVal) throws -> MalVal {
        switch key {
        case MalVal.MalSymbol(let str):
            data[str] = val
            return val
        default:
            throw MalError.General(msg: "invalid Env.find call")
        }
    }
}
