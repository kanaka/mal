
import Foundation

func READ(_ input: String) throws -> MalData {
    return try read_str(input)
}

func EVAL(_ ast: MalData, env: Env) throws -> MalData {
    switch ast.dataType {
    case .Vector:
        let vector = ast as! ContiguousArray<MalData>
        return try ContiguousArray(vector.map { element in try EVAL(element, env: env) })
    case .List:
        let list = ast as! [MalData]
        guard !list.isEmpty else { return list }
        guard let sym = list[0] as? Symbol else { throw MalError.Error }
        
        switch sym.name {
        case "def!":
            let value = try EVAL(list[2], env: env), key = list[1] as! Symbol
            env.set(value, forKey: key)
            return value
        case "let*":
            let newEnv = Env(outer: env), expr = list[2]
            let bindings = (list[1] is Vector<MalData>) ? List(list[1] as! Vector<MalData>) : list[1] as! List<MalData>
            for i in stride(from: 0, to: bindings.count-1, by: 2) {
                let key = bindings[i], value = bindings[i+1]
                let result = try EVAL(value, env: newEnv)
                newEnv.set(result, forKey: key as! Symbol)
            }
            return try EVAL(expr, env: newEnv)
        default:
            let evaluated = try eval_ast(list, env: env) as! [MalData]
            if let function = evaluated[0] as? Function {
                return try function.fn(List(evaluated.dropFirst()))
            } else {
                throw MalError.SymbolNotFound(list[0] as! Symbol)
            }
        }
    case .HashMap:
        let hashMap = ast as! HashMap<String, MalData>
        return try hashMap.mapValues { value in try EVAL(value, env: env) }
    default:
        return try eval_ast(ast, env: env)
    }
}

func PRINT(_ input: MalData) -> String {
    return pr_str(input, print_readably: true)
}

@discardableResult func rep(_ input: String, env: Env) throws -> String {

    return try PRINT(EVAL(READ(input), env: env))
}

func eval_ast(_ ast: MalData, env: Env) throws -> MalData {
    switch ast.dataType {
    case .Symbol:
        let sym = ast as! Symbol
        if let function =  try? env.get(forKey: sym) {
            return function
        } else {
            throw MalError.SymbolNotFound(sym)
        }
    case .List:
        let list = ast as! [MalData]
        return try list.map { element in try EVAL(element, env: env) }
    default:
        return ast
    }
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
