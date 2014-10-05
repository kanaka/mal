package printer

import (
    "fmt"
    "strings"
)

import (
    "types"
)

func _pr_list(lst []types.MalType, pr bool, start string, end string) string {
    str_list := make([]string, 0, len(lst))
    for _, e := range lst {
        str_list = append(str_list, Pr_str(e, pr))
    }
    return start + strings.Join(str_list, " ") + end
}

func Pr_str(obj types.MalType, print_readably bool) string {
    switch tobj := obj.(type) {
    case types.List:
        return _pr_list(tobj.Val, print_readably, "(", ")")
    case types.Vector:
        return _pr_list(tobj.Val, print_readably, "[", "]")
    case map[string]types.MalType:
        str_list := make([]string, 0, len(tobj)*2)
        for k, v := range tobj {
            str_list = append(str_list, Pr_str(k, print_readably))
            str_list = append(str_list, Pr_str(v, print_readably))
        }
        return "{" + strings.Join(str_list, " ") + "}"
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
}
