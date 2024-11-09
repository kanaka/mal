
import Foundation

func READ(_ input: String) throws -> MalData {
    return try read_str(input)
}

func EVAL(_ ast: MalData, env: [String: MalData]) throws -> MalData {
        /* print("EVAL: " + PRINT(ast)) */
        switch ast.dataType {
        case .List:
            let list = ast as! [MalData]
            guard !list.isEmpty else { return list }
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
            if let value = env[sym.name] {
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
