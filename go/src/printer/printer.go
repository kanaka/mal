package printer

import (
    "fmt"
    "strings"
)

import (
    "types"
)

func Pr_str(obj types.MalType, print_readably bool) string {
    switch tobj := obj.(type) {
    case types.List:
        str_list := make([]string, 0, len(tobj.Val))
        for _, e := range tobj.Val {
            str_list = append(str_list, Pr_str(e, print_readably))
        }
        return "(" + strings.Join(str_list, " ") + ")"
    case string:
        if print_readably {
            // TODO: quote backslash, quote, and newline
            return `"` + fmt.Sprintf("%v", obj) + `"`
        } else {
            return fmt.Sprintf("%v", obj)
        }
    case types.Symbol:
        return tobj.Val
    case nil:
        return "nil"
    default:
        return fmt.Sprintf("%v", obj)
    }
    return "<printed>"
}
