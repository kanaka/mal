import Foundation

public let keywordMagic: Character = "\u{029E}"

public enum Expr {
    case number(Int)
    case bool(Bool)
    case null
    case string(String)
    case symbol(String)
    indirect case list([Expr], Expr)
    indirect case vector([Expr], Expr)
    indirect case hashmap([String: Expr], Expr)
    case function(Func)
    case atom(Atom)
}

public extension Expr {
    static func list(_ arr: [Expr]) -> Expr {
        return .list(arr, .null)
    }

    static func vector(_ arr: [Expr]) -> Expr {
        return .vector(arr, .null)
    }

    static func hashmap(_ data: [String: Expr]) -> Expr {
        return .hashmap(data, .null)
    }
}

extension Expr: Equatable {
    public static func == (lhs: Self, rhs: Self) -> Bool {
        switch (lhs, rhs) {
        case let (.number(a), .number(b)):
            return a == b
        case let (.bool(a), .bool(b)):
            return a == b
        case (.null, .null):
            return true
        case let (.string(a), .string(b)):
            return a == b
        case let (.symbol(a), .symbol(b)):
            return a == b
        case let (.list(a), .list(b)),
             let (.vector(a), .vector(b)),
             let (.list(a), .vector(b)),
             let (.vector(a), .list(b)):
                return a == b
        case let (.hashmap(a), .hashmap(b)):
            return a == b
        case let (.function(a), .function(b)):
            return a == b
        case let (.atom(a), .atom(b)):
            return a == b

        default:
            return false
        }
    }
}

// MARK: - Func

final public class Func {
    public let run: ([Expr]) throws -> Expr
    public let ast: Expr?
    public let params: [String]
    public let env: Env?
    public let isMacro: Bool
    public let meta: Expr

    public init(
        ast: Expr? = nil,
        params: [String] = [],
        env: Env? = nil,
        isMacro: Bool = false,
        meta: Expr = .null,
        run: @escaping ([Expr]) throws -> Expr
    ) {
        self.run = run
        self.ast = ast
        self.params = params
        self.env = env
        self.isMacro = isMacro
        self.meta = meta
    }

    public func asMacros() -> Func {
        return Func(ast: ast, params: params, env: env, isMacro: true, meta: meta, run: run)
    }

    public func withMeta(_ meta: Expr) -> Func {
        return Func(ast: ast, params: params, env: env, isMacro: isMacro, meta: meta, run: run)
    }
}

extension Func: Equatable {
    public static func == (lhs: Func, rhs: Func) -> Bool {
        return lhs === rhs
    }
}

// MARK: - Atom

final public class Atom {
    public var val: Expr
    public let meta: Expr

    public init(_ val: Expr, meta: Expr = .null) {
        self.val = val
        self.meta = meta
    }

    public func withMeta(_ meta: Expr) -> Atom {
        return Atom(val, meta: meta)
    }
}

extension Atom: Equatable {
    public static func == (lhs: Atom, rhs: Atom) -> Bool {
        return lhs.val == rhs.val
    }
}
