import Foundation

// read
func READ(_ str: String) throws -> MalVal {
    return try read_str(str)
}

// eval
func EVAL(_ ast: MalVal, _ env: Env) throws -> MalVal {
    if let dbgeval = env.get("DEBUG-EVAL") {
        switch dbgeval {
        case MalVal.MalFalse, MalVal.MalNil: break
        default: print("EVAL: " + PRINT(ast))
        }
    }
    switch ast {
    case MalVal.MalSymbol(let sym):
        if let value = env.get(sym) {
            return value
        } else {
            throw MalError.General(msg: "'\(sym)' not found")
        }
    case MalVal.MalVector(let lst, _):
        return vector(try lst.map { try EVAL($0, env) })
    case MalVal.MalHashMap(let dict, _):
        var new_dict = Dictionary<String,MalVal>()
        for (k,v) in dict { new_dict[k] = try EVAL(v, env) }
        return hash_map(new_dict)
    case MalVal.MalList(let lst, _):
        if lst.count == 0 { return ast }
        switch lst[0] {
        case MalVal.MalSymbol("def!"):
            return try env.set(lst[1], try EVAL(lst[2], env))
        case MalVal.MalSymbol("let*"):
            let let_env = try Env(env)
            var binds = Array<MalVal>()
            switch lst[1] {
            case MalVal.MalList(let l, _): binds = l
            case MalVal.MalVector(let l, _): binds = l
            default:
                throw MalError.General(msg: "Invalid let* bindings")
            }
            var idx = binds.startIndex
            while idx < binds.endIndex {
                let v = try EVAL(binds[binds.index(after: idx)], let_env)
                try let_env.set(binds[idx], v)
                idx = binds.index(idx, offsetBy: 2)
            }
            return try EVAL(lst[2], let_env)
        default:
                let raw_args = lst[1..<lst.count]
                switch try EVAL(lst[0], env) {
                case MalVal.MalFunc(let fn, nil, _, _, _, _):
                    let args = try raw_args.map { try EVAL($0, env) }
                    return try fn(args)
                default:
                    throw MalError.General(msg: "Cannot apply on '\(lst[0])'")
                }
        }
    default:
        return ast
    }
}

// print
func PRINT(_ exp: MalVal) -> String {
    return pr_str(exp, true)
}


// repl
func rep(_ str:String) throws -> String {
    return PRINT(try EVAL(try READ(str), repl_env))
}

func IntOp(_ op: (Int, Int) -> Int, _ a: MalVal, _ b: MalVal) throws -> MalVal {
    switch (a, b) {
    case (MalVal.MalInt(let i1), MalVal.MalInt(let i2)):
        return MalVal.MalInt(op(i1, i2))
    default:
        throw MalError.General(msg: "Invalid IntOp call")
    }
}

var repl_env: Env = try Env()
try repl_env.set(MalVal.MalSymbol("+"),
                 malfunc({ try IntOp({ $0 + $1}, $0[0], $0[1]) }))
try repl_env.set(MalVal.MalSymbol("-"),
                 malfunc({ try IntOp({ $0 - $1}, $0[0], $0[1]) }))
try repl_env.set(MalVal.MalSymbol("*"),
                 malfunc({ try IntOp({ $0 * $1}, $0[0], $0[1]) }))
try repl_env.set(MalVal.MalSymbol("/"),
                 malfunc({ try IntOp({ $0 / $1}, $0[0], $0[1]) }))


while true {
    print("user> ", terminator: "")
    let line = readLine(strippingNewline: true)
    if line == nil { break }
    if line == "" { continue }

    do {
        print(try rep(line!))
    } catch (MalError.Reader(let msg)) {
        print("Error: \(msg)")
    } catch (MalError.General(let msg)) {
        print("Error: \(msg)")
    }
}
