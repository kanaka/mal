
import Foundation

func pr_str(_ input: MalData, print_readably: Bool) -> String {
    switch input.dataType {
    case .Symbol:
        let symbol = input as! Symbol
        return symbol.name
    case .Number:
        let number = input as! Number
        return String(number)
    case .True:
        return "true"
    case .False:
        return "false"
    case .Nil:
        return  "nil"
    case .Keyword:
        let keyword = input as! String
        return keyword.replacingCharacters(in: keyword.startIndex...keyword.startIndex, with: ":")
    case .String:
        let string = input as! String
        if print_readably {
            return "\"" + string.replacingOccurrences(of: "\\", with: "\\\\")
                                .replacingOccurrences(of: "\"", with: "\\\"")
                                .replacingOccurrences(of: "\n", with: "\\n") + "\""
        } else {
            return string
        }
    case .List:
        let list = input as! List<MalData>
        let stringOfElements = list.map { pr_str($0, print_readably: print_readably) }.joined(separator: " ")
        return "(" + stringOfElements + ")"
    case .Vector:
        let vector = input as! Vector<MalData>
        let stringOfElements = vector.map { pr_str($0, print_readably: print_readably) }.joined(separator: " ")
        return "[" + stringOfElements + "]"
    case .HashMap:
        let hashMap = input as! [String: MalData]
        let stringOfElements = hashMap.map { (key, value) in
            pr_str(key, print_readably: print_readably) + " " + pr_str(value, print_readably: print_readably)
        }.joined(separator: " ")
        return "{" + stringOfElements + "}"
    case .Atom:
        return pr_str("(atom \((input as! Atom).value))", print_readably: false)
    case .Function:
        return "#<function>"
    default:
        return "error type!"
    }
}
