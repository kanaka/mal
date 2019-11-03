import Foundation
import core

func read(_ s: String) throws -> Expr {
    return try Reader.read(s)
}

private func evalAst(_ expr: Expr, env: Env) throws -> Expr {
    switch expr {
    case let .symbol(name):
        return try env.get(name)
    case let .vector(values):
        return .vector(try values.map { try eval($0, env: env) })
    case let .hashmap(values):
        return .hashmap(try values.mapValues { try eval($0, env: env) })
    case let .list(ast):
        return .list(try ast.map { try eval($0, env: env) })
    default:
        return expr
    }
}

func eval(_ expr: Expr, env: Env) throws -> Expr {

    guard case let .list(ast) = expr else {
        return try evalAst(expr, env: env)
    }
    if ast.isEmpty {
        return expr
    }

    switch ast[0] {

    case .symbol("def!"):
        guard ast.count == 3 else { throw MalError.invalidArguments("def!") }
        guard case let .symbol(name) = ast[1] else { throw MalError.invalidArguments("def!") }

        let val = try eval(ast[2], env: env)
        env.set(forKey: name, val: val)
        return val

    case .symbol("let*"):
        guard ast.count == 3 else { throw MalError.invalidArguments("let*") }

        switch ast[1] {
        case let .list(bindable), let .vector(bindable):
            let letEnv = Env(outer: env)

            for i in stride(from: 0, to: bindable.count - 1, by: 2) {
                guard case let .symbol(key) = bindable[i] else { throw MalError.invalidArguments("let*") }
                let value = bindable[i + 1]
                letEnv.set(forKey: key, val: try eval(value, env: letEnv))
            }

            let expToEval = ast[2]
            return try eval(expToEval, env: letEnv)
        default:
            throw MalError.invalidArguments("let*")
        }

    case .symbol("do"):
        let exprsToEval = ast.dropFirst()
        if exprsToEval.isEmpty { throw MalError.invalidArguments("do") }
        return try exprsToEval.map { try eval($0, env: env) }.last!

    case .symbol("if"):
        guard 3...4 ~= ast.count else { throw MalError.invalidArguments("if") }

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

    case .symbol("fn*"):
        guard ast.count == 3 else { throw MalError("fn*") }
        let binds: [String]
        switch ast[1] {
        case let .list(xs), let .vector(xs):
            binds = try xs.map {
                guard case let .symbol(name) = $0 else { throw MalError.invalidArguments("fn*") }
                return name
            }
        default:
            throw MalError.invalidArguments("fn*")
        }

        let f = Func { args in
            let fEnv = try Env(binds: binds, exprs: args, outer: env)
            return try eval(ast[2], env: fEnv)
        }
        return .function(f)

    default:
        guard case let .list(evaluatedList) = try evalAst(expr, env: env) else { fatalError() }
        guard case let .function(fn) = evaluatedList[0] else { throw MalError("not a function: \(evaluatedList[0])") }
        return try fn.run(Array(evaluatedList.dropFirst()))
    }
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
