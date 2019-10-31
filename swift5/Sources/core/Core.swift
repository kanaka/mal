import Foundation

private extension Func {
    static func infixOperation(_ op: @escaping (Int, Int) -> Int) -> Func {
        return Func { args in
            guard args.count == 2,
                case let .number(a) = args[0],
                case let .number(b) = args[1] else { throw MalError("invalid arguments") }

            return .number(op(a, b))
        }
    }

    static func comparisonOperation(_ op: @escaping (Int, Int) -> Bool) -> Func {
        return Func { args in
            guard args.count == 2,
                case let .number(a) = args[0],
                case let .number(b) = args[1] else { throw MalError("invalid arguments") }

            return .bool(op(a, b))
        }
    }

    static let prn = Func { args in
        let printFunc = curry(Expr.print)(true)
        let result = args.map(printFunc).joined(separator: " ")
        print(result)
        return .null
    }

    static let str = Func { args in
        let printFunc = curry(Expr.print)(false)
        let result = args.map(printFunc).joined(separator: "")
        return .string(result)
    }

    static let prStr = Func { args in
        let printFunc = curry(Expr.print)(true)
        let result = args.map(printFunc).joined(separator: " ")
        return .string(result)
    }

    static let println = Func { args in
        let printFunc = curry(Expr.print)(false)
        let result = args.map(printFunc).joined(separator: " ")
        print(result)
        return .null
    }

    static let list = Func { args in .list(args) }

    static let isList = Func { args in
        if case .list = args.first {
            return .bool(true)
        }
        return .bool(false)
    }

    static let isEmpty = Func { args in
        switch args.first {
        case let .list(xs), let .vector(xs):
            return .bool(xs.isEmpty)
        default:
            return .bool(false)
        }
    }

    static let count = Func { args in
        switch args.first {
        case let .list(xs), let .vector(xs):
            return .number(xs.count)
        default:
            return .number(0)
        }
    }

    static let eq = Func { args in
        guard args.count == 2 else { throw MalError("eq: invalid arguments") }
        return args[0] == args[1] ? .bool(true) : .bool(false)
    }
}

private let data: [String: Expr] = [
    "+": .function(.infixOperation(+)),
    "-": .function(.infixOperation(-)),
    "*": .function(.infixOperation(*)),
    "/": .function(.infixOperation(/)),
    "prn": .function(.prn),
    "println": .function(.println),
    "pr-str": .function(.prStr),
    "str": .function(.str),
    "list": .function(.list),
    "list?": .function(.isList),
    "empty?": .function(.isEmpty),
    "count": .function(.count),
    "=": .function(.eq),
    "<": .function(.comparisonOperation(<)),
    "<=": .function(.comparisonOperation(<=)),
    ">": .function(.comparisonOperation(>)),
    ">=": .function(.comparisonOperation(>=))
]

public enum Core {
    public static let ns: Env = Env.init(data: data, outer: nil)
}
