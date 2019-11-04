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
        guard args.count == 2 else { throw MalError.invalidArguments("eq") }
        return args[0] == args[1] ? .bool(true) : .bool(false)
    }

    static let readString = Func { args in
        guard args.count == 1 else { throw MalError.invalidArguments("read-string") }
        guard case let .string(s) = args[0] else { throw MalError.invalidArguments("read-string") }
        return try Reader.read(s)
    }

    static let slurp = Func { args in
        guard args.count == 1 else { throw MalError.invalidArguments("slurp") }
        guard case let .string(filename) = args[0] else { throw MalError.invalidArguments("slurp") }
        return .string(try String(contentsOfFile: filename))
    }

    static let atom = Func { args in
        guard args.count == 1 else { throw MalError.invalidArguments("atom") }
        return .atom(Atom(args[0]))
    }

    static let isAtom = Func { args in
        guard args.count == 1 else { throw MalError.invalidArguments("atom?") }
        if case .atom = args[0] {
            return .bool(true)
        } else {
            return .bool(false)
        }
    }

    static let deref = Func { args in
        guard args.count == 1 else { throw MalError.invalidArguments("deref") }
        guard case let .atom(atom) = args[0] else { throw MalError.invalidArguments("deref") }
        return atom.val
    }

    static let reset = Func { args in
        guard args.count == 2 else { throw MalError.invalidArguments("reset!") }
        guard case let .atom(atom) = args[0] else { throw MalError.invalidArguments("reset!") }
        atom.val = args[1]
        return args[1]
    }

    static let swap = Func { args in
        guard args.count >= 2 else { throw MalError.invalidArguments("reset!") }
        guard case let .atom(atom) = args[0] else { throw MalError.invalidArguments("swap!") }
        guard case let .function(fn) = args[1] else { throw MalError.invalidArguments("swap!") }
        let otherArgs = args.dropFirst(2)
        atom.val = try fn.run([atom.val] + otherArgs)
        return atom.val
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
    ">=": .function(.comparisonOperation(>=)),
    "read-string": .function(.readString),
    "slurp": .function(.slurp),
    "atom": .function(.atom),
    "atom?": .function(.isAtom),
    "deref": .function(.deref),
    "reset!": .function(.reset),
    "swap!": .function(.swap)
]

public enum Core {
    public static let ns: Env = Env.init(data: data, outer: nil)
}
