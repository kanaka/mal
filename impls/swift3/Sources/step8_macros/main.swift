import Foundation

// read
func READ(_ str: String) throws -> MalVal {
    return try read_str(str)
}

// eval

func starts_with(_ ast: MalVal, _ sym: String) -> MalVal? {
    switch ast {
    case MalVal.MalList(let lst, _) where 1 < lst.count:
        switch lst[0] {
        case MalVal.MalSymbol(sym):
            return lst[1]
        default:
            return nil
        }
    default:
        return nil
    }
}

func qqIter(_ lst: [MalVal]) -> MalVal {
    var result = list([])
    for elt in lst.reversed() {
         if let elt1 = starts_with(elt, "splice-unquote") {
             result = list([MalVal.MalSymbol("concat"), elt1, result])
         } else {
             result = list([MalVal.MalSymbol("cons"), quasiquote(elt), result])
         }
    }
    return result
}

func quasiquote(_ ast: MalVal) -> MalVal {
    if let a1 = starts_with(ast, "unquote") {
        return a1
    }
    switch ast {
    case MalVal.MalList(let lst, _):
        return qqIter(lst)
    case MalVal.MalVector(let lst, _):
        return list([MalVal.MalSymbol("vec"), qqIter(lst)])
    case MalVal.MalSymbol:
        return list([MalVal.MalSymbol("quote"), ast])
    case MalVal.MalHashMap:
        return list([MalVal.MalSymbol("quote"), ast])
    default:
        return ast
    }
}

func EVAL(_ orig_ast: MalVal, _ orig_env: Env) throws -> MalVal {
  var ast = orig_ast, env = orig_env
  while true {
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
            env = let_env
            ast = lst[2] // TCO
        case MalVal.MalSymbol("quote"):
            return lst[1]
        case MalVal.MalSymbol("quasiquote"):
            ast = quasiquote(lst[1]) // TCO
        case MalVal.MalSymbol("defmacro!"):
            var mac = try EVAL(lst[2], env)
            switch mac {
            case MalVal.MalFunc(let fn, let a, let e, let p, _, let m):
                mac = malfunc(fn,ast:a,env:e,params:p,macro:true,meta:m)
            default: throw MalError.General(msg: "invalid defmacro! form")
            }
            return try env.set(lst[1], mac)
        case MalVal.MalSymbol("do"):
            let slc = lst[1..<lst.index(before: lst.endIndex)]
            for item in slc {
                _ = try EVAL(item, env)
            }
            ast = lst[lst.index(before: lst.endIndex)] // TCO
        case MalVal.MalSymbol("if"):
            switch try EVAL(lst[1], env) {
            case MalVal.MalFalse, MalVal.MalNil:
                if lst.count > 3 {
                    ast = lst[3] // TCO
                } else {
                    return MalVal.MalNil
                }
            default:
                ast = lst[2] // TCO
            }
        case MalVal.MalSymbol("fn*"):
            return malfunc( {
                return try EVAL(lst[2], Env(env, binds: lst[1],
                                                 exprs: list($0)))
            }, ast:[lst[2]], env:env, params:[lst[1]])
        default:
                let raw_args = lst[1..<lst.count]
                switch try EVAL(lst[0], env) {
                case MalVal.MalFunc(let fn, _, _, _, true, _):
                    ast = try fn(Array(raw_args)) // TCO
                case MalVal.MalFunc(let fn, nil, _, _, _, _):
                    let args = try raw_args.map { try EVAL($0, env) }
                    return try fn(args)
                case MalVal.MalFunc(_, let a, let e, let p, _, _):
                    let args = try raw_args.map { try EVAL($0, env) }
                    env = try Env(e, binds: p![0],
                                     exprs: list(args)) // TCO
                    ast = a![0] // TCO
                default:
                    throw MalError.General(msg: "Cannot apply on '\(lst[0])'")
                }
        }
    default:
        return ast
    }
  }
}

// print
func PRINT(_ exp: MalVal) -> String {
    return pr_str(exp, true)
}


// repl
@discardableResult
func rep(_ str:String) throws -> String {
    return PRINT(try EVAL(try READ(str), repl_env))
}

var repl_env: Env = try Env()

// core.swift: defined using Swift
for (k, fn) in core_ns {
    try repl_env.set(MalVal.MalSymbol(k), malfunc(fn))
}
try repl_env.set(MalVal.MalSymbol("eval"),
                 malfunc({ try EVAL($0[0], repl_env) }))
let pargs = CommandLine.arguments.map { MalVal.MalString($0) }
// TODO: weird way to get empty list, fix this
var args = pargs[pargs.startIndex..<pargs.startIndex]
if pargs.index(pargs.startIndex, offsetBy:2) < pargs.endIndex {
    args = pargs[pargs.index(pargs.startIndex, offsetBy:2)..<pargs.endIndex]
}
try repl_env.set(MalVal.MalSymbol("*ARGV*"), list(Array(args)))

// core.mal: defined using the language itself
try rep("(def! not (fn* (a) (if a false true)))")
try rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")
try rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")


if CommandLine.arguments.count > 1 {
    try rep("(load-file \"" + CommandLine.arguments[1] + "\")")
    exit(0)
}

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
    } catch (MalError.MalException(let obj)) {
        print("Error: \(pr_str(obj, true))")
    }
}
