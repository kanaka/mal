import Foundation

// read
func READ(str: String) throws -> MalVal {
    return try read_str(str)
}

// eval
func is_pair(ast: MalVal) -> Bool {
    switch ast {
    case MalVal.MalList(let lst, _):   return lst.count > 0
    case MalVal.MalVector(let lst, _): return lst.count > 0
    default:                           return false
    }
}

func quasiquote(ast: MalVal) -> MalVal {
    if !is_pair(ast) {
        return list([MalVal.MalSymbol("quote"), ast])
    }
    let a0 = try! _nth(ast, 0)
    switch a0 {
    case MalVal.MalSymbol("unquote"):
        return try! _nth(ast, 1)
    default: true // fallthrough
    }
    if is_pair(a0) {
        let a00 = try! _nth(a0, 0)
        switch a00 {
        case MalVal.MalSymbol("splice-unquote"):
            return list([MalVal.MalSymbol("concat"),
                        try! _nth(a0, 1),
                        quasiquote(try! rest(ast))])
        default: true // fallthrough
        }
    }

    return list([MalVal.MalSymbol("cons"),
                 quasiquote(a0),
                 quasiquote(try! rest(ast))])
}

func is_macro(ast: MalVal, _ env: Env) -> Bool {
    switch ast {
    case MalVal.MalList(let lst, _) where lst.count > 0:
        let a0 = lst[lst.startIndex]
        switch a0 {
        case MalVal.MalSymbol:
            let e = try! env.find(a0)
            if e != nil {
                let mac = try! e!.get(a0)
                switch mac {
                case MalVal.MalFunc(_,_,_,_,let macro,_): return macro
                default: return false
                }
            } else {
                return false
            }
        default: return false
        }
    default: return false
    }
}

func macroexpand(orig_ast: MalVal, _ env: Env) throws -> MalVal {
    var ast: MalVal = orig_ast
    while is_macro(ast, env) {
        switch try! env.get(try! _nth(ast, 0)) {
        case MalVal.MalFunc(let mac,_,_,_,_,_):
            ast = try mac(_rest(ast))
        default: throw MalError.General(msg: "impossible state in macroexpand")
        }
    }
    return ast
}

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

func EVAL(orig_ast: MalVal, _ orig_env: Env) throws -> MalVal {
  var ast = orig_ast, env = orig_env
  while true {
    switch ast {
    case MalVal.MalList(let lst, _): if lst.count == 0 { return ast }
    default: return try eval_ast(ast, env)
    }

    ast = try macroexpand(ast, env)
    switch ast {
    case MalVal.MalList: true
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
        case MalVal.MalSymbol("macroexpand"):
            return try macroexpand(lst[1], env)
        case MalVal.MalSymbol("try*"):
            do {
                return try EVAL(_nth(ast, 1), env)
            } catch (let exc) {
                if lst.count > 2 {
                    let a2 = lst[2]
                    switch a2 {
                    case MalVal.MalList(let a2lst, _):
                        let a20 = a2lst[0]
                        switch a20 {
                        case MalVal.MalSymbol("catch*"):
                            if a2lst.count < 3 { return MalVal.MalNil }
                            let a21 = a2lst[1], a22 = a2lst[2]
                            var err: MalVal
                            switch exc {
                            case MalError.Reader(let msg):
                                err = MalVal.MalString(msg)
                            case MalError.General(let msg):
                                err = MalVal.MalString(msg)
                            case MalError.MalException(let obj):
                                err = obj
                            default:
                                err = MalVal.MalString(String(exc))
                            }
                            return try EVAL(a22, Env(env, binds: list([a21]),
                                                          exprs: list([err])))
                        default: true // fall through
                        }
                    default: true // fall through
                    }
                }
                throw exc
            }
        case MalVal.MalSymbol("do"):
            let slc = lst[1..<lst.endIndex.predecessor()]
            try eval_ast(list(Array(slc)), env)
            ast = lst[lst.endIndex.predecessor()] // TCO
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
            switch try eval_ast(ast, env) {
            case MalVal.MalList(let elst, _):
                switch elst[0] {
                case MalVal.MalFunc(let fn, nil, _, _, _, _):
                    let args = Array(elst[1..<elst.count])
                    return try fn(args)
                case MalVal.MalFunc(_, let a, let e, let p, _, _):
                    let args = Array(elst[1..<elst.count])
                    env = try Env(e, binds: p![0],
                                     exprs: list(args)) // TCO
                    ast = a![0] // TCO
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
}

// print
func PRINT(exp: MalVal) -> String {
    return pr_str(exp, true)
}


// repl
func rep(str:String) throws -> String {
    return PRINT(try EVAL(try READ(str), repl_env))
}

var repl_env: Env = try Env()

// core.swift: defined using Swift
for (k, fn) in core_ns {
    try repl_env.set(MalVal.MalSymbol(k), malfunc(fn))
}
try repl_env.set(MalVal.MalSymbol("eval"),
                 malfunc({ try EVAL($0[0], repl_env) }))
let pargs = Process.arguments.map { MalVal.MalString($0) }
// TODO: weird way to get empty list, fix this
var args = pargs[pargs.startIndex..<pargs.startIndex]
if pargs.startIndex.advancedBy(2) < pargs.endIndex {
    args = pargs[pargs.startIndex.advancedBy(2)..<pargs.endIndex]
}
try repl_env.set(MalVal.MalSymbol("*ARGV*"), list(Array(args)))

// core.mal: defined using the language itself
try rep("(def! not (fn* (a) (if a false true)))")
try rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))")
try rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")
try rep("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))")


if Process.arguments.count > 1 {
    try rep("(load-file \"" + Process.arguments[1] + "\")")
    exit(0)
}

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
