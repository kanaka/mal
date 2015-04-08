//******************************************************************************
// MAL - env
//******************************************************************************

import Foundation

typealias EnvironmentVars = [MalSymbol: MalVal]

let kSymbolAmpersand = MalSymbol(symbol: "&")
let kNil = MalNil()
let kNilSymbol = MalSymbol(symbol: "")

class Environment {
    init(outer: Environment?) {
        self.outer = outer
    }

    func set_bindings(binds: MalSequence, with_exprs exprs: MalSequence) -> MalVal {
        for var index = 0; index < binds.count; ++index {
            if !is_symbol(binds[index]) { return MalError(message: "an entry in binds was not a symbol: index=\(index), binds[index]=\(binds[index])") }
            let sym = binds[index] as! MalSymbol
            if sym != kSymbolAmpersand {
                if index < exprs.count {
                    set(sym, exprs[index])
                } else {
                    set(sym, kNil)
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
            let rest_sym = binds[index--] as! MalSymbol
            let rest = exprs[index..<exprs.count]
            set(rest_sym, MalList(slice: rest))
            break
        }
        return kNil
    }

    func set(sym: MalSymbol, _ value: MalVal) -> MalVal {
        if num_bindings == 0 {
            slot_name0 = sym; slot_value0 = value; ++num_bindings
        } else if num_bindings == 1 {
            if slot_name0 == sym { slot_value0 = value }
            else { slot_name1 = sym; slot_value1 = value; ++num_bindings }
        } else if num_bindings == 2 {
            if slot_name0 == sym { slot_value0 = value }
            else if slot_name1 == sym { slot_value1 = value }
            else { slot_name2 = sym; slot_value2 = value; ++num_bindings }
        } else if num_bindings == 3 {
            if slot_name0 == sym { slot_value0 = value }
            else if slot_name1 == sym { slot_value1 = value }
            else if slot_name2 == sym { slot_value2 = value }
            else { slot_name3 = sym; slot_value3 = value; ++num_bindings }
        } else if num_bindings == 4 {
            if slot_name0 == sym { slot_value0 = value }
            else if slot_name1 == sym { slot_value1 = value }
            else if slot_name2 == sym { slot_value2 = value }
            else if slot_name3 == sym { slot_value3 = value }
            else {
                data[slot_name0] = slot_value0
                data[slot_name1] = slot_value1
                data[slot_name2] = slot_value2
                data[slot_name3] = slot_value3
                data[sym] = value; ++num_bindings
            }
        } else {
            data[sym] = value
        }
        return value
    }

    func get(sym: MalSymbol) -> MalVal? {
        if num_bindings > 4 { if let val = data[sym] { return val } }
        if num_bindings > 3 { if slot_name3 == sym { return slot_value3 } }
        if num_bindings > 2 { if slot_name2 == sym { return slot_value2 } }
        if num_bindings > 1 { if slot_name1 == sym { return slot_value1 } }
        if num_bindings > 0 { if slot_name0 == sym { return slot_value0 } }
        return outer?.get(sym)
    }

    private var outer: Environment?
    private var data = EnvironmentVars()
    private var num_bindings = 0
    private var slot_name0: MalSymbol = kNilSymbol
    private var slot_name1: MalSymbol = kNilSymbol
    private var slot_name2: MalSymbol = kNilSymbol
    private var slot_name3: MalSymbol = kNilSymbol
    private var slot_value0: MalVal = kNil
    private var slot_value1: MalVal = kNil
    private var slot_value2: MalVal = kNil
    private var slot_value3: MalVal = kNil
}
