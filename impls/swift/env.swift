//******************************************************************************
// MAL - env
//******************************************************************************

import Foundation

typealias EnvironmentVars = [MalSymbol: MalVal]

private let kSymbolAmpersand = as_symbol(make_symbol("&"))
private let kSymbolNil       = as_symbol(make_symbol(""))
private let kNil             = make_nil()

final class Environment {
    init(outer: Environment?) {
        self.outer = outer
    }

    func set_bindings(binds: MalSequence, with_exprs exprs: MalSequence) throws -> MalVal {
        for var index: MalIntType = 0; index < binds.count; ++index {
            guard let sym = as_symbolQ(try! binds.nth(index)) else {
                try throw_error("an entry in binds was not a symbol: index=\(index), binds[index]=\(try! binds.nth(index))")
            }
            if sym != kSymbolAmpersand {
                if index < exprs.count {
                    set(sym, try! exprs.nth(index))
                } else {
                    set(sym, kNil)
                }
                continue
            }

            guard (index + 1) < binds.count else {
                try throw_error("found & but no symbol")
            }
            guard let rest_sym = as_symbolQ(try! binds.nth(index + 1)) else {
                try throw_error("& was not followed by a symbol: index=\(index), binds[index]=\(try! binds.nth(index))")
            }
            let rest = exprs.range_from(index, to: exprs.count)
            set(rest_sym, rest)
            break
        }
        return kNil
    }

    // In this implementation, rather than storing everything in a dictionary,
    // we optimize for small environments by having a hard-coded set of four
    // slots. We use these slots when creating small environments, such as when
    // a function is invoked. Testing shows that supporting up to four variables
    // in this way is a good trade-off. Otherwise, if we have more than four
    // variables, we switch over to using a dictionary. Testing also shows that
    // trying to use both the slots and the dictionary for large environments is
    // not as efficient as just completely switching over to the dictionary.
    //
    // Interestingly, even though the MalVal return value is hardly ever used at
    // the call site, removing it and returning nothing is a performance loss.
    // This is because returning 'value' allows the compiler to skip calling
    // swift_release on it. The result is that set() calls swift_release twice
    // (on self and sym), as opposed to three times (on self, sym, and value) if
    // it were to return something other than one of the parameters.

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
        if num_bindings > 4 { if let val = data[sym] { return val }; return outer?.get(sym) }
        if num_bindings > 3 { if slot_name3 == sym { return slot_value3 } }
        if num_bindings > 2 { if slot_name2 == sym { return slot_value2 } }
        if num_bindings > 1 { if slot_name1 == sym { return slot_value1 } }
        if num_bindings > 0 { if slot_name0 == sym { return slot_value0 } }
        return outer?.get(sym)
    }

    private var outer: Environment?
    private var data = EnvironmentVars()
    private var num_bindings = 0
    private var slot_name0 = kSymbolNil
    private var slot_name1 = kSymbolNil
    private var slot_name2 = kSymbolNil
    private var slot_name3 = kSymbolNil
    private var slot_value0 = kNil
    private var slot_value1 = kNil
    private var slot_value2 = kNil
    private var slot_value3 = kNil
}
