//******************************************************************************
// MAL - printer
//******************************************************************************

import Foundation

func with_print_readably<T>(print_readably: Bool, fn: () -> T) -> T {
    let old = MalValPrintReadably
    MalValPrintReadably = print_readably
    let result = fn()
    MalValPrintReadably = old
    return result
}

func pr_str(m: MalVal, print_readably: Bool) -> String {
    return with_print_readably(print_readably) {
        m.description
    }
}
