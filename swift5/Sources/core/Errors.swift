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
    public static func arityMismath(name: String, expected: String, given: Int) -> MalError {
        let message = """
            \(name): arity mismatch
            expected: \(expected)
            given: \(given)
        """
        return MalError(message)
    }

    public static func unbalanced(expected: String) -> MalError {
        return MalError("unbalanced: expected \(expected)")
    }

    public static func unbalanced(unexpected: String) -> MalError {
        return MalError("unbalanced: unexpected \(unexpected)")
    }

    public static func invalidArguments(_ name: String) -> MalError {
        return MalError("\(name): invalid arguments")
    }

    public static func outOfRange() -> MalError {
        return MalError("index out of range")
    }
}

extension Expr: Error, LocalizedError {
    public var errorDescription: String? {
        return "Error: \(self)"
    }
}
