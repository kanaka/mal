import Foundation

// read
func READ(str: String) throws -> MalVal {
    return try read_str(str)
}

// eval
func eval_ast(ast: MalVal, _ env: Env) throws -> MalVal {
    switch ast {
    case MalVal.MalSymbol:
        return try env.get(ast)
    case MalVal.MalList(let lst, _):
        return list(try lst.map { try EVAL($0, env) })
    case MalVal.MalVector(let lst, _):
        return vector(try lst.map { try EVAL($0, env) })
    case MalVal.MalHashMap(let dict, _):
        var new_dict = Dictionary<String,MalVal>()
        for (k,v) in dict { new_dict[k] = try EVAL(v, env) }
        return hash_map(new_dict)
    default:
        return ast
    }
}

func EVAL(ast: MalVal, _ env: Env) throws -> MalVal {
    switch ast {
    case MalVal.MalList(let lst, _): if lst.count == 0 { return ast }
    default: return try eval_ast(ast, env)
    }

    switch ast {
    case MalVal.MalList(let lst, _):
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
                let v = try EVAL(binds[idx.successor()], let_env)
                try let_env.set(binds[idx], v)
                idx = idx.successor().successor()
            }
            return try EVAL(lst[2], let_env)
        default:
            switch try eval_ast(ast, env) {
            case MalVal.MalList(let elst, _):
                switch elst[0] {
                case MalVal.MalFunc(let fn,_,_,_,_,_):
                    let args = Array(elst[1..<elst.count])
                    return try fn(args)
                default:
                    throw MalError.General(msg: "Cannot apply on '\(elst[0])'")
                }
            default: throw MalError.General(msg: "Invalid apply")
            }
        }
    default:
        throw MalError.General(msg: "Invalid apply")
    }
}

// print
func PRINT(exp: MalVal) -> String {
    return pr_str(exp, true)
}


// repl
func rep(str:String) throws -> String {
    return PRINT(try EVAL(try READ(str), repl_env))
}

func IntOp(op: (Int, Int) -> Int, _ a: MalVal, _ b: MalVal) throws -> MalVal {
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
    let line = readLine(stripNewline: true)
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
