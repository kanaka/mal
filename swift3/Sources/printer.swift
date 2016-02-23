
func pr_str(obj: MalVal, _ print_readably: Bool = true) -> String {
    switch obj {
    case MalVal.MalList(let lst):
        let elems = lst.map { pr_str($0, print_readably) }
        return "(" + elems.joinWithSeparator(" ")  + ")"
    case MalVal.MalVector(let lst):
        let elems = lst.map { pr_str($0, print_readably) }
        return "[" + elems.joinWithSeparator(" ")  + "]"
    case MalVal.MalString(let str):
        if print_readably {
            let s1 = str.stringByReplacingOccurrencesOfString(
                "\\", withString: "\\\\")
            let s2 = s1.stringByReplacingOccurrencesOfString(
                "\"", withString: "\\\"")
            let s3 = s2.stringByReplacingOccurrencesOfString(
                "\n", withString: "\\n")
            return "\"" + s3 + "\""
        } else {
            return str
        }
    case MalVal.MalSymbol(let str):
        return str
    case MalVal.MalInt(let i): return String(i)
    case MalVal.MalNil:        return "nil"
    case MalVal.MalFalse:      return "false"
    case MalVal.MalTrue:       return "true"
    case MalVal.MalFunc(_, nil, _, _):
        return "#<native function>"
    case MalVal.MalFunc(_, let ast, _, let params):
        return "(fn* \(pr_str(params![0])) \(pr_str(ast![0])))"
    default:                   return String(obj)
    }
}
