//******************************************************************************
// MAL - printer
//******************************************************************************

import Foundation

var MalValPrintReadably = true

func with_print_readably<T>(print_readably: Bool, fn: () -> T) -> T {
    let old = MalValPrintReadably
    MalValPrintReadably = print_readably
    let result = fn()
    MalValPrintReadably = old
    return result
}

func pr_str(m: MalVal, _ print_readably: Bool = MalValPrintReadably) -> String {
    return with_print_readably(print_readably) {
        if is_string(m) {
            return print_readably ? escape(m.description) : m.description
        }
        if is_keyword(m) {
            return ":\(m.description)"
        }
        return m.description
    }
}
