import Foundation
import core

extension Func {

    static fileprivate func infixOperation(_ op: @escaping (Int, Int) -> Int) -> Func {
        return Func { args in
            guard args.count == 2,
                case let .number(a) = args[0],
                case let .number(b) = args[1] else { throw MalError.invalidArguments() }

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

            let expToEval = ast[2]
            return try eval(expToEval, env: letEnv)
        default:
            throw MalError.invalidArguments("let*")
        }

    default:
        guard case let .list(ast, _) = try evalAst(expr, env: env) else { fatalError() }
        guard case let .function(fn) = ast[0] else { throw MalError.invalidFunctionCall(ast[0]) }
        return try fn.run(Array(ast.dropFirst()))
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

while true {
    print("user> ", terminator: "")
    guard let s = readLine() else { break }
    print(rep(s, env: replEnv))
}
