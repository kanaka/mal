import Foundation
import core

extension Func {

    static fileprivate func infixOperation(_ op: @escaping (Int, Int) -> Int) -> Func {
        return Func { args in
            guard args.count == 2,
                case let .number(a) = args[0],
                case let .number(b) = args[1] else { throw MalError("invalid arguments") }

            return .number(op(a, b))
        }
    }
}

let replEnv: Environment = [
    "+": .function(.infixOperation(+)),
    "-": .function(.infixOperation(-)),
    "*": .function(.infixOperation(*)),
    "/": .function(.infixOperation(/))
]

func read(_ s: String) throws -> Expr {
    return try Reader.read(s)
}

func eval(_ expr: Expr, env: Environment) throws -> Expr {
    switch expr {
    case let .symbol(name):
        guard let value = env[name] else { throw MalError("unknown symbol \(name)") }
        return value
    case let .vector(values):
        return .vector(try values.map { try eval($0, env: env) })
    case let .hashmap(values):
        return .hashmap(try values.mapValues { try eval($0, env: env) })
    case .list:
        return try eval_list(expr, env: env)
    default:
        return expr
    }
}

func eval_list(_ expr: Expr, env: Environment) throws -> Expr {
    guard case let .list(values) = expr else { fatalError() }

    if values.isEmpty {
        return expr
    }

    let evaluated = try values.map { try eval($0, env: env) }
    guard case let .function(fn) = evaluated.first else { throw MalError("not a function: \(evaluated.first!)") }
    return try fn.run(Array(evaluated.dropFirst()))
}

func print(_ expr: Expr) -> String {
    return Expr.print(expr)
}

func rep(_ s: String, env: Environment) -> String {
    do {
        let expr = try read(s)
        let resExpr = try eval(expr, env: env)
        let resultStr = print(resExpr)
        return resultStr
    } catch {
        return error.localizedDescription
    }
}

while true {
    print("user> ", terminator: "")
    guard let s = readLine() else { break }
    print(rep(s, env: replEnv))
}
