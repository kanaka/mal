import Foundation

extension Expr {
    public static func print(readable: Bool = true, _ expr: Expr) -> String {

        let print = curry(Self.print)(readable)

        switch expr {
        case let .number(value):
            return "\(value)"
        case let .list(arr):
            let inner: String = arr.map(print).joined(separator: " ")
            return "(" + inner + ")"
        case let .vector(arr):
            let inner: String = arr.map(print).joined(separator: " ")
            return "[" + inner + "]"
        case let .hashmap(m):
            let inner = m.map { print($0.key) + " " + print($0.value) }.joined(separator: " ")
            return "{" + inner + "}"
        case let .string(s):
            var inner = s
            if readable {
                inner = s
                    .replacingOccurrences(of: "\\", with: "\\\\")
                    .replacingOccurrences(of: "\n", with: "\\n")
                    .replacingOccurrences(of: "\"", with: "\\\"")
            }
            return "\"" + inner + "\""
        case let .symbol(s):
            return s
        case let .keyword(s):
            return ":" + s
        }
    }
}

extension Expr: CustomDebugStringConvertible {
    public var debugDescription: String {
        Expr.print(self)
    }
}

private func curry<A, B, C>(_ function: @escaping (A, B) -> C) -> (A) -> (B) -> C {
    return { (a: A) -> (B) -> C in { (b: B) -> C in function(a, b) } }
}
