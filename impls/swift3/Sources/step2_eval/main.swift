import Foundation

// read
func READ(_ str: String) throws -> MalVal {
    return try read_str(str)
}

// eval
func EVAL(_ ast: MalVal, _ env: Dictionary<String, MalVal>) throws -> MalVal {
    /* print("EVAL: " + PRINT(ast)) */
    switch ast {
    case MalVal.MalSymbol(let sym):
        if let value = env[sym] {
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

                let raw_args = lst[1..<lst.count]
                switch try EVAL(lst[0], env) {
                case MalVal.MalFunc(let fn, nil, _, _, _, _):
                    let args = try raw_args.map { try EVAL($0, env) }
                    return try fn(args)
                default:
                    throw MalError.General(msg: "Cannot apply on '\(lst[0])'")
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

var repl_env: Dictionary<String,MalVal> = [
    "+": malfunc({ try IntOp({ $0 + $1}, $0[0], $0[1]) }),
    "-": malfunc({ try IntOp({ $0 - $1}, $0[0], $0[1]) }),
    "*": malfunc({ try IntOp({ $0 * $1}, $0[0], $0[1]) }),
    "/": malfunc({ try IntOp({ $0 / $1}, $0[0], $0[1]) }),
]

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
