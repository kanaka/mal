import Foundation

public let keywordMagic: Character = "\u{029E}"

public enum Expr {
    case number(Int)
    case bool(Bool)
    case null
    case string(String)
    case symbol(String)
    case list([Expr])
    case vector([Expr])
    case hashmap([String: Expr])
    case function(Func)
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

        default:
            return false
        }
    }
}

public class Func {
    public let run: ([Expr]) throws -> Expr
    public let ast: Expr?
    public let params: [String]
    public let env: Env?

    public init(
        ast: Expr? = nil,
        params: [String] = [],
        env: Env? = nil,
        run: @escaping ([Expr]) throws -> Expr
    ) {
        self.run = run
        self.ast = ast
        self.params = params
        self.env = env
    }
}

extension Func: Equatable {
    public static func == (lhs: Func, rhs: Func) -> Bool {
        return lhs === rhs
    }
}
