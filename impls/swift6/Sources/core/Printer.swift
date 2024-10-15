import Foundation

extension Expr {

    public static func print(readable: Bool = true, _ expr: Expr) -> String {

        let print = curry(Self.print)(readable)

        switch expr {
        case let .number(value):
            return "\(value)"
        case let .list(arr, _):
            let inner: String = arr.map(print).joined(separator: " ")
            return "(" + inner + ")"
        case let .vector(arr, _):
            let inner: String = arr.map(print).joined(separator: " ")
            return "[" + inner + "]"
        case let .hashmap(m, _):
            let inner = m.map { printString($0.key, readable: readable) + " " + print($0.value) }.joined(separator: " ")
            return "{" + inner + "}"
        case let .string(s):
            return printString(s, readable: readable)
        case let .symbol(s):
            return s
        case let .bool(b):
            return b ? "true" : "false"
        case .null:
            return "nil"
        case let .function(fn):
            return fn.isMacro ? "#<macro-function>" : "#<function>"
        case let .atom(expr):
            return "(atom \(print(expr.val)))"
        }
    }
}

private func printString(_ s: String, readable: Bool) -> String {
    if s.first == keywordMagic {
        return ":" + s.dropFirst()
    }
    return readable ? ("\"" + unescape(s) + "\"") : s
}

private func unescape(_ s: String) -> String {
    return s
        .replacingOccurrences(of: "\\", with: "\\\\")
        .replacingOccurrences(of: "\n", with: "\\n")
        .replacingOccurrences(of: "\"", with: "\\\"")
}

extension Expr: CustomDebugStringConvertible {
    public var debugDescription: String {
        Expr.print(self)
    }
}
