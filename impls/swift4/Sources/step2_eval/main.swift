
import Foundation

func READ(_ input: String) throws -> MalData {
    return try read_str(input)
}

func EVAL(_ ast: MalData, env: [String: MalData]) throws -> MalData {
    switch ast.dataType {
    case .Vector:
        let vector = ast as! ContiguousArray<MalData>
        return try ContiguousArray(vector.map { element in try EVAL(element, env: env) })
    case .List:
        let list = ast as! [MalData]
        if list.isEmpty { return list }
        let evaluated = try eval_ast(list, env: env) as! [MalData]
        if let function = evaluated[0] as? Function {
            return try function.fn(List(evaluated.dropFirst()))
        } else {
            throw MalError.SymbolNotFound(list[0] as! Symbol)
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

@discardableResult func rep(_ input: String) throws -> String{
    func calculate(_ args: [MalData], op: (Number, Number) -> Number) throws -> MalData {
        guard args.count == 2, args[0] is Number, args[1] is Number else { throw MalError.InvalidArgument }
        return op(args[0] as! Number, args[1] as! Number)
    }
    
    let repl_env = ["+": Function(fn: { args in try calculate(args, op: +) }),
                    "-": Function(fn: { args in try calculate(args, op: -) }),
                    "*": Function(fn: { args in try calculate(args, op: *) }),
                    "/": Function(fn: { args in try calculate(args, op: /) })]

    return try PRINT(EVAL(READ(input), env: repl_env))
}

func eval_ast(_ ast: MalData, env: [String: MalData]) throws -> MalData {
    switch ast.dataType {
    case .Symbol:
        let sym = ast as! Symbol
        if let function =  env[sym.name] {
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




while true {
    print("user> ", terminator: "")
    if let input = readLine(strippingNewline: true) {
        guard input != "" else { continue }
        do {
            try print(rep(input))
        } catch let error as MalError {
            print(error.info())
        }
    } else {
        exit(0);
    }
}
