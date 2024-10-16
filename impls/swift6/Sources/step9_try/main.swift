import Foundation
import core

func read(_ s: String) throws -> Expr {
    return try Reader.read(s)
}

private func qq_loop(_ elt: Expr, acc: Expr) throws -> Expr {
    if case let .list(xs, _) = elt {
        if 0 < xs.count && xs[0] == .symbol("splice-unquote") {
            guard xs.count == 2 else { throw MalError.invalidArguments("splice-unquote") }
            return .list([.symbol("concat"), xs[1], acc])
        }
    }
    return .list([.symbol("cons"), try quasiquote(elt), acc])
}
private func qq_foldr(_ xs: [Expr]) throws -> Expr {
    var acc : Expr = .list([])
    for i in stride(from: xs.count-1, through: 0, by: -1) {
        acc = try qq_loop(xs[i], acc:acc)
    }
    return acc
}
private func quasiquote(_ expr: Expr) throws -> Expr {
    switch expr {
    case let .list(xs, _):
        if 0 < xs.count && xs[0] == .symbol("unquote") {
            guard xs.count == 2 else { throw MalError.invalidArguments("unquote") }
            return xs[1]
        } else {
            return try qq_foldr(xs)
        }
    case let .vector(xs, _):
        return .list([.symbol("vec"), try qq_foldr(xs)])
    case .symbol(_), .hashmap(_):
        return .list([.symbol("quote"), expr])
    default:
        return expr
    }
}

func eval(_ expr: Expr, env: Env) throws -> Expr {

    var env = env
    var expr = expr
    while true {

    switch env.get("DEBUG-EVAL") {
        case nil, .bool(false), .null: break
        default: print("EVAL: " + print(expr))
    }

    switch expr {
    case let .symbol(name):
        let val = env.get(name)
        guard val != nil else { throw MalError.symbolNotFound(name) }
        return val!
    case let .vector(values, _):
        return .vector(try values.map { try eval($0, env: env) })
    case let .hashmap(values, _):
        return .hashmap(try values.mapValues { try eval($0, env: env) })
    case let .list(ast, _):

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
                return try eval(ast[1], env: env)
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
            guard case let .function(fn) = try eval(ast[0], env: env) else { throw MalError.invalidFunctionCall(ast[0]) }
            if fn.isMacro {
                expr = try fn.run(Array(ast.dropFirst()))
                continue
            }
            let args = try ast.dropFirst().map { try eval($0, env: env) }
            if let ast = fn.ast, let fnEnv = fn.env {
                let newEnv = try Env(binds: fn.params, exprs: args, outer: fnEnv)
                env = newEnv
                expr = ast
            } else {
                return try fn.run(args)
            }
        }
    default:
        return expr
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

replEnv.set(forKey: "eval", val: .function(Func { args in
    guard let expr = args.first else { throw MalError.invalidArguments("eval") }
    return try eval(expr, env: replEnv)
}))
replEnv.set(forKey: "*ARGV*", val: .list(CommandLine.arguments.dropFirst(2).map(Expr.string)))

_ = rep("(def! not (fn* (a) (if a false true)))", env: replEnv)
_ = rep(#"(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))"#, env: replEnv)
_ = rep(#"(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons 'cond (rest (rest xs)))))))"#, env: replEnv)

if CommandLine.arguments.count > 1 {
    _ = rep("(load-file \"" + CommandLine.arguments[1] + "\")", env: replEnv)
    exit(0)
}

while true {
    print("user> ", terminator: "")
    guard let s = readLine() else { break }
    print(rep(s, env: replEnv))
}
