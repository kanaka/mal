import Foundation
import core

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

private func isTaggedList(_ ast: [Expr], tagName: String) -> Bool {
    if case let .symbol(name) = ast.first, name == tagName {
        return true
    }
    return false
}

private func isDefinition(_ ast: [Expr]) -> Bool {
    return isTaggedList(ast, tagName: "def!")
}

private func evalDefinition(_ ast: [Expr], env: Env) throws -> Expr {
    guard ast.count == 3 else { throw MalError("def!: invalid arguments") }
    guard case let .symbol(name) = ast[1] else { throw MalError("def!: invalid arguments") }

    let expToEval = ast[2]
    let val = try eval(expToEval, env: env)
    env.set(forKey: name, val: val)
    return val
}

private func isLetForm(_ ast: [Expr]) -> Bool {
    return isTaggedList(ast, tagName: "let*")
}

private func evalLetForm(_ ast: [Expr], env: Env) throws -> Expr {
    guard ast.count == 3 else { throw MalError("let*: invalid arguments") }

    switch ast[1] {
    case let .list(bindable), let .vector(bindable):
        let letEnv = Env(outer: env)

        for i in stride(from: 0, to: bindable.count - 1, by: 2) {
            guard case let .symbol(key) = bindable[i] else { throw MalError("let*: invalid arguments") }
            let value = bindable[i + 1]
            letEnv.set(forKey: key, val: try eval(value, env: letEnv))
        }

        let expToEval = ast[2]
        return try eval(expToEval, env: letEnv)
    default:
        throw MalError("let*: invalid arguments")
    }
}

private func isDoForm(_ ast: [Expr]) -> Bool {
    return isTaggedList(ast, tagName: "do")
}

private func evalDoForm(_ ast: [Expr], env: Env) throws -> Expr {
    let exprsToEval = ast.dropFirst()
    if exprsToEval.isEmpty { throw MalError("do: invalid arguments") }

    return try exprsToEval.map { try eval($0, env: env) }.last!
}

private func isIfForm(_ ast: [Expr]) -> Bool {
    return isTaggedList(ast, tagName: "if")
}

private func evalIfForm(_ ast: [Expr], env: Env) throws -> Expr {
    guard 3...4 ~= ast.count else { throw MalError("if: invalid arguments") }

    let condExpr = ast[1]
    switch try eval(condExpr, env: env) {
    case .bool(false), .null:
        if let falseExpr = ast[safe: 3] {
            return try eval(falseExpr, env: env)
        }
        return .null
    default:
        return try eval(ast[2], env: env)
    }
}

private func isFnForm(_ values: [Expr]) -> Bool {
    return isTaggedList(values, tagName: "fn*")
}

private func evalFnForm(_ ast: [Expr], env: Env) throws -> Expr {
    guard ast.count == 3 else { throw MalError("fn*: invalid arguments") }
    let binds: [String]
    switch ast[1] {
    case let .list(xs), let .vector(xs):
        binds = try xs.map {
            guard case let .symbol(name) = $0 else { throw MalError("fn*: invalid arguments") }
            return name
        }
    default:
        throw MalError("fn*: invalid arguments")
    }

    let f = Func { args in
        let fEnv = try Env(binds: binds, exprs: args, outer: env)
        return try eval(ast[2], env: fEnv)
    }
    return .function(f)
}

func eval_list(_ expr: Expr, env: Env) throws -> Expr {
    guard case let .list(ast) = expr else { fatalError() }

    if ast.isEmpty {
        return expr
    }

    if isDefinition(ast) {
        return try evalDefinition(ast, env: env)
    }

    if isLetForm(ast) {
        return try evalLetForm(ast, env: env)
    }

    if isDoForm(ast) {
        return try evalDoForm(ast, env: env)
    }

    if isIfForm(ast) {
        return try evalIfForm(ast, env: env)
    }

    if isFnForm(ast) {
        return try evalFnForm(ast, env: env)
    }

    let evaluated = try ast.map { try eval($0, env: env) }
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

let replEnv: Env = Env(data: Core.ns.data)

_ = rep("(def! not (fn* (a) (if a false true)))", env: replEnv)

while true {
    print("user> ", terminator: "")
    guard let s = readLine() else { break }
    print(rep(s, env: replEnv))
}
