import Foundation
import core

func read(_ s: String) throws -> Expr {
    return try Reader.read(s)
}

private func isPair(_ expr: Expr) -> Bool {
    switch expr {
    case let .list(values, _), let .vector(values, _):
        return !values.isEmpty
    default:
        return false
    }
}

private func asListOrVector(_ expr: Expr) -> [Expr]? {
    switch expr {
    case let .list(values, _), let .vector(values, _):
        return values
    default:
        return nil
    }
}

private func quasiquote(_ expr: Expr) throws -> Expr {
    if !isPair(expr) {
        return .list([.symbol("quote"), expr])
    }
    guard let ast = asListOrVector(expr), !ast.isEmpty else {
        throw MalError.invalidArguments("quasiquote")
    }

    if case .symbol("unquote") = ast[0] {
        guard ast.count > 1 else { throw MalError.invalidArguments("unquote") }
        return ast[1]
    }

    if isPair(ast[0]), let ast0 = asListOrVector(ast[0]) {
        if case .symbol("splice-unquote") = ast0.first {
            guard ast0.count > 1 else { throw MalError.invalidArguments("splice-unquote") }
            let rest = try quasiquote(.list(Array(ast[1...])))
            return .list([.symbol("concat"), ast0[1], rest])
        }
    }

    let rest = try quasiquote(.list(Array(ast[1...])))
    return .list([.symbol("cons"), try quasiquote(ast[0]), rest])
}

private func macroExpand(_ expr: Expr, env: Env) throws -> Expr {
    var expr = expr
    while true {
        guard case let .list(ast, _) = expr,
            case let .symbol(name) = ast.first,
            case let .function(fn) = try? env.get(name),
            fn.isMacro else {
                break
        }

        expr = try fn.run(Array(ast.dropFirst()))
    }
    return expr
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

        expr = try macroExpand(expr, env: env)

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

        case .symbol("quote"):
            guard ast.count == 2 else { throw MalError.invalidArguments("quote") }
            return ast[1]

        case .symbol("quasiquote"):
            guard ast.count == 2 else { throw MalError.invalidArguments("quasiquote") }
            expr = try quasiquote(ast[1])

        case .symbol("defmacro!"):
            guard ast.count == 3 else { throw MalError.invalidArguments("defmacro!") }
            guard case let .symbol(name) = ast[1] else { throw MalError.invalidArguments("defmacro!") }

            guard case let .function(fn) = try eval(ast[2], env: env) else { throw MalError.invalidArguments("defmacro!") }
            let macros = fn.asMacros()
            env.set(forKey: name, val: .function(macros))
            return .function(macros)

        case .symbol("macroexpand"):
            guard ast.count == 2 else { throw MalError.invalidArguments("macroexpand") }
            return try macroExpand(ast[1], env: env)

        case .symbol("try*"):
            if ast.count == 2 {
                expr = ast[1]
                continue
            }
            guard ast.count == 3 else { throw MalError.invalidArguments("try*") }
            guard case let .list(values, _) = ast[2], values.count == 3 else { throw MalError.invalidArguments("try*") }
            guard case .symbol("catch*") = values[0] else { throw MalError.invalidArguments("try*") }
            guard case let .symbol(bind) = values[1] else { throw MalError.invalidArguments("catch*") }

            do {
                expr = try eval(ast[1], env: env)
            } catch {
                let malErr = (error as? Expr) ?? .string(error.localizedDescription)
                let newEnv = try Env(binds: [bind], exprs: [malErr], outer: env)
                env = newEnv
                expr = values[2]
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

@discardableResult
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

replEnv.set(forKey: "eval", val: .function(Func { args in
    guard let expr = args.first else { throw MalError.invalidArguments("eval") }
    return try eval(expr, env: replEnv)
}))
replEnv.set(forKey: "*ARGV*", val: .list(CommandLine.arguments.dropFirst(2).map(Expr.string)))
replEnv.set(forKey: "*host-language*", val: .string("swift5"))

rep("(def! not (fn* (a) (if a false true)))", env: replEnv)
rep(#"(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))"#, env: replEnv)
rep(#"(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons 'cond (rest (rest xs)))))))"#, env: replEnv)

if CommandLine.arguments.count > 1 {
    rep("(load-file \"" + CommandLine.arguments[1] + "\")", env: replEnv)
    exit(0)
}

rep(#"(println (str "Mal [" *host-language* "]"))"#, env: replEnv)

while true {
    print("user> ", terminator: "")
    guard let s = readLine() else { break }
    print(rep(s, env: replEnv))
}
