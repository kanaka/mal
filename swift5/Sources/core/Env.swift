import Foundation

public class Env {
    private var outer: Env?
    public private(set) var data: [String: Expr]

    public init(data: [String: Expr] = [:], outer: Env? = nil) {
        self.outer = outer
        self.data = data
    }

    public init(binds: [String], exprs: [Expr], outer: Env? = nil) throws {
        self.outer = outer
        self.data = [:]

        for i in 0..<binds.count {
            let bindName = binds[i]
            if bindName == "&" {
                guard let key = binds[safe: i + 1] else { throw MalError("invalid variadic function definition") }
                data[key] = .list(Array(exprs[i...]))
                break
            }
            guard let exp = exprs[safe: i] else { throw MalError("function call: invalid arguments") }
            data[bindName] = exp
        }
    }

    public func set(forKey key: String, val: Expr) {
        data[key] = val
    }

    public func get(_ key: String) throws -> Expr {
        guard let val = find(key) else { throw MalError("\(key) not found") }
        return val
    }

    private func find(_ key: String) -> Expr? {
        if let val = data[key] {
            return val
        }
        if let outer = outer {
            return outer.find(key)
        }
        return nil
    }
}
