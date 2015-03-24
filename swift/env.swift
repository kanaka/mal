//******************************************************************************
// MAL - env
//******************************************************************************

import Foundation

typealias EnvironmentVars = [MalSymbol: MalVal]

let kSymbolAmpersand = MalSymbol(symbol: "&")

class Environment {
    init(outer: Environment?) {
        self.outer = outer
    }

    func set_bindings(binds: MalSequence, with_exprs exprs: MalSequence) -> MalVal {
        for var index = 0; index < binds.count; ++index {
            if !is_symbol(binds[index]) { return MalError(message: "an entry in binds was not a symbol: index=\(index), binds[index]=\(binds[index])") }
            let sym = binds[index] as MalSymbol
            if sym != kSymbolAmpersand {
                if index < exprs.count {
                    set(sym, exprs[index])
                } else {
                    set(sym, MalNil())
                }
                continue
            }
            // I keep getting messed up by the following, so here's an
            // explanation. We are iterating over two lists, and are at this
            // point:
            // 
            //           index
            //             |
            //             v
            // binds: (... & name)
            // exprs: (... a b    c d e ...)
            //
            // In the following, we increment index to get to "name", and then
            // later decrement it to get to (a b c d e ...)
            if ++index >= binds.count { return MalError(message: "found & but no symbol") }
            if !is_symbol(binds[index]) { return MalError(message: "& was not followed by a symbol: index=\(index), binds[index]=\(binds[index])") }
            let rest_sym = binds[index--] as MalSymbol
            let rest = exprs[index..<exprs.count]
            set(rest_sym, MalList(slice: rest))
            break
        }
        return MalNil()
    }

    func set(sym: MalSymbol, _ value: MalVal) -> MalVal {
        data[sym] = value
        return value
    }

    func get(sym: MalSymbol) -> MalVal? {
        if let val = data[sym] { return val }
        return outer?.get(sym)
    }

    private var outer: Environment?
    private var data = EnvironmentVars()
}
