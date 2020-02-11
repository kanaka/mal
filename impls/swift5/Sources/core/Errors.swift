import Foundation

public struct MalError: Error, LocalizedError {
    let message: String

    public init(_ message: String) {
        self.message = message
    }

    public var errorDescription: String? {
        "\(message)"
    }
}

extension MalError {
    public static func unbalanced(expected: String) -> MalError {
        return MalError("unbalanced: expected \(expected)")
    }

    public static func unbalanced(unexpected: String) -> MalError {
        return MalError("unbalanced: unexpected \(unexpected)")
    }

    public static func invalidArguments(_ name: String) -> MalError {
        return MalError("\(name): invalid arguments")
    }

    public static func invalidArguments() -> MalError {
        return MalError("invalid arguments")
    }

    public static func outOfRange() -> MalError {
        return MalError("index out of range")
    }

    public static func invalidFunctionCall(_ expr: Expr) -> MalError {
        return MalError("not a function: \(expr)")
    }

    public static func symbolNotFound(_ s: String) -> MalError {
        return MalError("'\(s)' not found")
    }

    public static func invalidVariadicFunction() -> MalError {
        return MalError("invalid variadic function definition")
    }

    public static func reader() -> MalError {
        return MalError("can't parse")
    }
}

extension Expr: Error, LocalizedError {
    public var errorDescription: String? {
        return "Error: \(self)"
    }
}
