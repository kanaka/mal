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

var replEnv: Env = Env()
replEnv.set(forKey: "+", val: .function(.infixOperation(+)))
replEnv.set(forKey: "-", val: .function(.infixOperation(-)))
replEnv.set(forKey: "*", val: .function(.infixOperation(*)))
replEnv.set(forKey: "/", val: .function(.infixOperation(/)))

func read(_ s: String) throws -> Expr {
    return try Reader.read(s)
}

func eval(_ expr: Expr, env: Env) throws -> Expr {
    switch expr {
    case let .symbol(name):
        let value = try env.get(name)
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

private func isTaggedList(_ values: [Expr], tagName: String) -> Bool {
    if case let .symbol(name) = values.first, name == tagName {
        return true
    }
    return false
}

private func isDefinition(_ values: [Expr]) -> Bool {
    return isTaggedList(values, tagName: "def!")
}

private func evalDefinition(_ values: [Expr], env: Env) throws -> Expr {
    guard values.count == 3 else { throw MalError("def!: invalid arguments") }
    guard case let .symbol(name) = values[1] else { throw MalError("def!: invalid arguments") }

    let expToEval = values[2]
    let val = try eval(expToEval, env: env)
    env.set(forKey: name, val: val)
    return val
}

private func isLetForm(_ values: [Expr]) -> Bool {
    return isTaggedList(values, tagName: "let*")
}

private func evalLetForm(_ values: [Expr], env: Env) throws -> Expr {
    guard values.count == 3 else { throw MalError("let*: invalid arguments") }

    switch values[1] {
    case let .list(bindable), let .vector(bindable):
        let letEnv = Env(outer: env)

        for i in stride(from: 0, to: bindable.count - 1, by: 2) {
            guard case let .symbol(key) = bindable[i] else { throw MalError("let*: invalid arguments") }
            let value = bindable[i + 1]
            letEnv.set(forKey: key, val: try eval(value, env: letEnv))
        }

        let expToEval = values[2]
        return try eval(expToEval, env: letEnv)
    default:
        throw MalError("let*: invalid arguments")
    }
}

func eval_list(_ expr: Expr, env: Env) throws -> Expr {
    guard case let .list(values) = expr else { fatalError() }

    if values.isEmpty {
        return expr
    }

    if isDefinition(values) {
        return try evalDefinition(values, env: env)
    }

    if isLetForm(values) {
        return try evalLetForm(values, env: env)
    }

    let evaluated = try values.map { try eval($0, env: env) }
    guard case let .function(fn) = evaluated.first else { throw MalError("not a function: \(evaluated.first!)") }
    return try fn.run(Array(evaluated.dropFirst()))
}

func print(_ expr: Expr) -> String {
    return Expr.print(expr)
}

func rep(_ s: String, env: Env) -> String {
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
