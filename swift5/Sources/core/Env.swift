import Foundation

public struct Env {
    private var _outer: [Env]
    private var outer: Env? {
        return _outer.first
    }

    private var data: [String: Expr]

    public init(data: [String: Expr] = [:], outer: Env? = nil) {
        self._outer = outer != nil ? [outer!] : []
        self.data = data
    }

    public mutating func set(forKey key: String, val: Expr) {
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
