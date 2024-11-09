
import Foundation

func READ(_ input: String) throws -> MalData {
    return try read_str(input)
}

func EVAL(_ ast: MalData, env: Env) throws -> MalData {
        if let dbgeval = try? env.get(forKey: Symbol("DEBUG-EVAL")) {
            if ![.False, .Nil].contains(dbgeval.dataType) {
                print("EVAL: " + PRINT(ast))
            }
        }
        switch ast.dataType {
        case .List:
            let list = ast as! [MalData]
            guard !list.isEmpty else { return list }
            if let sym = list[0] as? Symbol {
                switch sym.name {
                case "def!":
                    let value = try EVAL(list[2], env: env), key = list[1] as! Symbol
                    env.set(value, forKey: key)
                    return value
                case "let*":
                    let newEnv = Env(outer: env), expr = list[2]
                    let bindings = list[1].listForm
                    for i in stride(from: 0, to: bindings.count-1, by: 2) {
                        let key = bindings[i], value = bindings[i+1]
                        let result = try EVAL(value, env: newEnv)
                        newEnv.set(result, forKey: key as! Symbol)
                    }
                    return try EVAL(expr, env: newEnv)
                default:
                    break
                }
            }
            // not a symbol. maybe: function, list, or some wrong type
            guard let function = try EVAL(list[0], env: env) as? Function else {
                throw MalError.SymbolNotFound(list[0] as? Symbol ?? Symbol("Symbol"))
            }
            let raw_args = list.dropFirst()
            let args = try raw_args.map { try EVAL($0, env: env) }
            return try function.fn(args)
        case .Vector:
            let vector = ast as! ContiguousArray<MalData>
            return try ContiguousArray(vector.map { element in try EVAL(element, env: env) })
        case .HashMap:
            let hashMap = ast as! HashMap<String, MalData>
            return try hashMap.mapValues { value in try EVAL(value, env: env) }
        case .Symbol:
            let sym = ast as! Symbol
            if let value = try? env.get(forKey: sym) {
                return value
            } else {
                throw MalError.SymbolNotFound(sym)
            }
        default:
            return ast
        }
}

func PRINT(_ input: MalData) -> String {
    return pr_str(input, print_readably: true)
}

@discardableResult func rep(_ input: String, env: Env) throws -> String {
    return try PRINT(EVAL(READ(input), env: env))
}

func calculate(_ args: [MalData], op: (Number, Number) -> Number) throws -> MalData {
    guard args.count == 2, args[0] is Number, args[1] is Number else { throw MalError.InvalidArgument }
    return op(args[0] as! Number, args[1] as! Number)
}
let repl_env = Env()
repl_env.set(Function(fn: { args in try calculate(args, op: +) }), forKey: Symbol("+"))
repl_env.set(Function(fn: { args in try calculate(args, op: -) }), forKey: Symbol("-"))
repl_env.set(Function(fn: { args in try calculate(args, op: *) }), forKey: Symbol("*"))
repl_env.set(Function(fn: { args in try calculate(args, op: /) }), forKey: Symbol("/"))

while true {
    print("user> ", terminator: "")
    if let input = readLine(strippingNewline: true) {
        guard input != "" else { continue }
        do {
            try print(rep(input, env: repl_env))
        } catch let error as MalError {
            print(error.info())
        }
    } else {
        exit(0);
    }
}
