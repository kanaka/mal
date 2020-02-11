
import Foundation

class Env {
    let outer: Env?
    var data: [String: MalData] = [:]
    
    init(outer: Env) {
        self.outer = outer
    }
    init() {
        outer = nil
    }
    init(binds: [Symbol], exprs: [MalData], outer: Env) {
        self.outer = outer
        self.data = [:]
        for i in binds.indices {
            if binds[i].name == "&" {
                data.updateValue(List(exprs[i..<exprs.count]), forKey: binds[i+1].name)
                return
            }
            data.updateValue(exprs[i], forKey: binds[i].name)
        }
    }
    
    func set(_ value: MalData, forKey key:Symbol) {
        data.updateValue(value, forKey: key.name)
    }
    func find(_ key: Symbol) -> Env? {
        if let _ = data[key.name] {
            return self
        } else  {
            return outer?.find(key)
        }
    }
    func get(forKey key: Symbol) throws -> MalData {
        if let env = find(key), let value = env.data[key.name] {
            return value
        } else {
            throw MalError.SymbolNotFound(key)
        }
    }
}
