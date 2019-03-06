
import Foundation

func calculate(_ args: [MalData], op: (Number, Number) -> Number) throws -> MalData {
    guard args.count == 2, args[0] is Number, args[1] is Number else {
        throw MalError.InvalidArgument
    }
    return op(args[0] as! Number, args[1] as! Number)
}

func isEqualList(_ l: [MalData], _ r: [MalData]) -> Bool {
    guard l.count == r.count else {
        return false
    }
    for i in l.indices {
        if !isEqual(l[i], r[i]) { return false }
    }
    return true
}

func isEqualHashMap (_ l: [String: MalData], _ r: [String: MalData]) -> Bool {
    guard l.count == r.count else {
        return false
    }
    for key in l.keys {
        guard let lValue = l[key], let rValue = r[key] else { return false }
        if !isEqual(lValue, rValue) { return false }
    }
    return true
}

func isEqual(_ l: MalData, _ r: MalData) -> Bool {
    switch (l.dataType, r.dataType) {
    case (.Symbol, .Symbol):
        return (l as! Symbol).name == (r as! Symbol).name
    case (.String, .String), (.Keyword, .Keyword):
        return (l as! String) == (r as! String)
    case (.Number, .Number):
        return (l as! Number) == (r as! Number)
    case (.List, .List), (.Vector, .Vector), (.List, .Vector), (. Vector, .List):
        return isEqualList(l.listForm, r.listForm)
    case (.HashMap, .HashMap):
        return isEqualHashMap((l as! [String: MalData]), (r as! [String: MalData]))
    case (.Nil, .Nil), (.True, .True), (.False, .False):
        return true
    default: // atom, function
        return false
    }
}

func hashMap(fromList list: [MalData]) throws -> [String: MalData] {
    var hashMap: [String: MalData] = [:]
    for index in stride(from: 0, to: list.count, by: 2) {
        guard list[index] is String, index+1 < list.count else { throw MalError.Error }
        hashMap.updateValue(list[index+1], forKey: list[index] as! String)
    }
    return hashMap
}

let ns: [String: ([MalData]) throws -> MalData] =
    ["+":       { try calculate($0, op: +) },
     "-":       { try calculate($0, op: -) },
     "*":       { try calculate($0, op: *) },
     "/":       { try calculate($0, op: /) },
     "<":       { args in (args[0] as! Number) <  (args[1] as! Number) },
     ">":       { args in (args[0] as! Number) >  (args[1] as! Number) },
     "<=":      { args in (args[0] as! Number) <= (args[1] as! Number) },
     ">=":      { args in (args[0] as! Number) >= (args[1] as! Number) },
     
     "=":       { args in let left = args[0], right = args[1]; return isEqual(left, right) },
     
     "pr-str":  { $0.map { pr_str($0, print_readably: true)}.joined(separator: " ") },
     "str":     { $0.map { pr_str($0, print_readably: false)}.joined(separator: "") },
     "prn":     { print($0.map { pr_str($0, print_readably: true)}.joined(separator: " ")); return Nil() },
     "println": { print($0.map { pr_str($0, print_readably: false)}.joined(separator: " ")); return Nil() },
     
     "list":    { List($0) },
     "list?":   { let param = $0[0]; return param is [MalData] },
     "empty?":  { $0[0].count == 0 },
     "count":   { $0[0].count },
     
     "read-string": { try read_str($0[0] as! String) },
     "slurp":   { try String(contentsOfFile: $0[0] as! String) },
     
     "atom":    { Atom($0[0]) },
     "atom?":   { $0[0] is Atom },
     "deref":   { ($0[0] as? Atom)?.value ?? Nil() },
     "reset!":  { args in (args[0] as! Atom).value = args[1]; return args[1] },
     "swap!":   { args in
        let atom = args[0] as! Atom, fn = args[1] as! Function,
            others = args.dropFirst(2).listForm
        atom.value = try fn.fn([atom.value] + others)
        return atom.value
        },
     "cons":    { args in [args[0]] + args[1].listForm },
     "concat":  { $0.reduce([]) { (result, array ) in result + array.listForm } },
     
     "nth":     { args in
        let list = args[0].listForm, i = args[1] as! Int
        guard list.indices.contains(i) else { throw MalError.IndexOutOfBounds }
        return list[i]
        },
     "first":   { $0[0].listForm.first ?? Nil() },
     "rest":    { $0[0].listForm.dropFirst().listForm },
        
     "throw":   { throw MalError.MalException($0[0]) },
     "apply":   { args in
        let fn = args[0] as! Function
        let newArgs = args.dropFirst().dropLast().listForm + args.last!.listForm
        return try fn.fn(newArgs)
        },
     "map":     { args in
        let fn = args[0] as! Function
        let closure = fn.fn
        var result: [MalData] = []
        for element in args[1].listForm {
            result.append(try fn.fn([element])) }
        return result
        },
     
     "nil?":    { $0[0] is Nil },
     "true?":   { $0[0].dataType == .True },
     "false?":  { $0[0].dataType == .False },
     "symbol?": { $0[0].dataType == .Symbol },
     "symbol":  { Symbol($0[0] as! String) },
     "keyword": { ($0[0].dataType == .Keyword) ? $0[0] : "\u{029E}" + ($0[0] as! String) },
     "keyword?":{ $0[0].dataType == .Keyword },
     "vector":  { Vector($0) },
     "vector?": { $0[0].dataType == .Vector },
     "hash-map":{ try hashMap(fromList: $0) },
     "map?":    { $0[0].dataType == .HashMap },
     "assoc":   {
        let map = $0[0] as! [String: MalData]
        return map.merging(try hashMap(fromList: $0.dropFirst().listForm)) { (_, new) in new }
        },
     "dissoc":  { args in
        let map = args[0] as! [String: MalData]
        return map.filter { (key, _) in !(args.dropFirst().listForm as! [String]).contains(key) }
        },
     "get":     {
        if let map = $0[0] as? [String: MalData] {
            return map[$0[1] as! String] ?? Nil() }
        return Nil()
        },
     "contains?": { ($0[0] as! [String: MalData])[$0[1] as! String] != nil },
     "keys":    {
        ($0[0] as! [String: MalData]).reduce([]) { result, element in
            let (key, _) = element
            return result + [key] }
        },
     "vals":    {
        ($0[0] as! [String: MalData]).reduce([]) { result, element in
            let (_, value) = element
            return result + [value] }
        },
     "sequential?": { [.List, .Vector].contains($0[0].dataType) },

     "readline": {
        print($0[0] as! String, terminator: "")
        return readLine(strippingNewline: true) ?? Nil() },
     
     "meta":    {
        switch $0[0].dataType {
        case .Function:
            return ($0[0] as! Function).meta ?? Nil()
        default:
            return Nil()
        }},
     "with-meta": {
        switch $0[0].dataType {
        case .Function:
            return Function(withFunction: $0[0] as! Function, meta: $0[1])
        default:
            return $0[0]
        }},
     "time-ms": { _ in Int(Date().timeIntervalSince1970 * 1000) },
     "conj":    {
        if let list = $0[0] as? [MalData] {
            return $0.dropFirst().reversed().listForm + list
        } else { // vector
            return ($0[0] as! Vector) + Vector($0.dropFirst())
        }},
     "string?": { $0[0].dataType == .String },
     "number?": { $0[0].dataType == .Number },
     "fn?":     {
        if let fn = $0[0] as? Function {
            return !fn.isMacro
        } else {
            return false
        }},
     "macro?":  {
        if let fn = $0[0] as? Function {
            return fn.isMacro
        } else {
            return false
        }},
     "seq":     {
        if $0[0].count == 0 { return Nil() }
        switch $0[0].dataType {
        case .List:
            return $0[0] as! List<MalData>
        case .Vector:
            return List($0[0] as! ContiguousArray<MalData>)
        case .String:
            return List($0[0] as! String).map { String($0) }
        default:
            return Nil()
        }},
]
