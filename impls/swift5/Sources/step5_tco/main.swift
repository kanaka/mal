import Foundation
import core

func read(_ s: String) throws -> Expr {
    return try Reader.read(s)
}

private func evalAst(_ expr: Expr, env: Env) throws -> Expr {
    switch expr {
    case let .symbol(name):
        return try env.get(name)
    case let .vector(values, _):
        return .vector(try values.map { try eval($0, env: env) })
    case let .hashmap(values, _):
        return .hashmap(try values.mapValues { try eval($0, env: env) })
    case let .list(ast, _):
        return .list(try ast.map { try eval($0, env: env) })
    default:
        return expr
    }
}

func eval(_ expr: Expr, env: Env) throws -> Expr {

    var env = env
    var expr = expr

    while true {

        guard case let .list(ast, _) = expr else {
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
            case let .list(bindable, _), let .vector(bindable, _):
                let letEnv = Env(outer: env)

                for i in stride(from: 0, to: bindable.count - 1, by: 2) {
                    guard case let .symbol(key) = bindable[i] else { throw MalError.invalidArguments("let*") }
                    let value = bindable[i + 1]
                    letEnv.set(forKey: key, val: try eval(value, env: letEnv))
                }

                expr = ast[2]
                env = letEnv
            default:
                throw MalError.invalidArguments("let*")
            }

        case .symbol("do"):
            let exprsToEval = ast.dropFirst()
            guard !exprsToEval.isEmpty else { throw MalError.invalidArguments("do") }
            _ = try exprsToEval.dropLast().map { try eval($0, env: env) }
            expr = exprsToEval.last!

        case .symbol("if"):
            guard 3...4 ~= ast.count else { throw MalError.invalidArguments("if") }

            switch try eval(ast[1], env: env) {
            case .bool(false), .null:
                if let falseBranch = ast[safe: 3] {
                    expr = falseBranch
                } else {
                    expr = .null
                }
            default:
                expr = ast[2]
            }

        case .symbol("fn*"):
            guard ast.count == 3 else { throw MalError.invalidArguments("fn*") }
            let binds: [String]

            switch ast[1] {
            case let .list(xs, _), let .vector(xs, _):
                binds = try xs.map {
                    guard case let .symbol(name) = $0 else { throw MalError.invalidArguments("fn*") }
                    return name
                }
            default:
                throw MalError.invalidArguments("fn*")
            }

            let run: ([Expr]) throws -> Expr = { args in
                let fEnv = try Env(binds: binds, exprs: args, outer: env)
                return try eval(ast[2], env: fEnv)
            }

            let f = Func(ast: ast[2], params: binds, env: env, run: run)
            return .function(f)

        default:
            guard case let .list(ast, _) = try evalAst(expr, env: env) else { fatalError() }
            guard case let .function(fn) = ast[0] else { throw MalError.invalidFunctionCall(ast[0]) }

            let args = Array(ast.dropFirst())
            if let ast = fn.ast, let fnEnv = fn.env {
                let newEnv = try Env(binds: fn.params, exprs: args, outer: fnEnv)
                env = newEnv
                expr = ast
            } else {
                return try fn.run(args)
            }
        }
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
