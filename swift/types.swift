//******************************************************************************
// MAL - types
//******************************************************************************
//
//  CLASS TREE:             NOTES:
//  ---------------------   ----------------------------------------------------
//  MalVal
//  ├── MalNil              ┐
//  ├── MalTrue             │ May make these subclasses of MalConstant
//  ├── MalFalse            ┘
//  ├── MalInteger          ┐ May make these subclasses of MalNumber along
//  ├── MalFloat            ┘ with MalRatio and MalComplex
//  ├── MalSymbol
//  ├── MalKeyword
//  ├── MalString
//  ├── MalSequence         Allows us to use either List or Vector in the same context.
//  │   ├── MalList
//  │   └── MalVector
//  ├── MalHashMap
//  ├── MalAtom
//  ├── MalFunction         Allows us to uniformly invoke either a built-in or a closure.
//  │   ├── MalClosure
//  │   └── MalBuiltin
//  ├── MalComment
//  └── MalError            Indicates an error in parsing or executing.
//
import Foundation

// ===== MalVal =====

var MalValPrintReadably = true

class MalVal : Printable, Hashable {
    enum MalType : String {
        case TypeUnknown    = "Unknown"
        case TypeNil        = "Nil"
        case TypeTrue       = "True"
        case TypeFalse      = "False"
        case TypeInteger    = "Integer"
        case TypeFloat      = "Float"
        case TypeSymbol     = "Symbol"
        case TypeKeyword    = "Keyword"
        case TypeString     = "String"
        case TypeList       = "List"
        case TypeVector     = "Vector"
        case TypeHashMap    = "HashMap"
        case TypeAtom       = "Atom"
        case TypeClosure    = "Closure"
        case TypeBuiltin    = "Builtin"
        case TypeComment    = "Comment"
        case TypeError      = "Error"
    }

    init(meta: MalVal? = nil) { self.meta = meta }
    init(other: MalVal) { self.meta = other.meta }
    func clone() -> MalVal { return MalVal(other:self) }
    var type: MalType { return .TypeUnknown }
    var description: String { return type.rawValue }
    var hashValue: Int { return description.hashValue }
    var meta: MalVal?
    //class var print_readably = true   // Swift does not yet support class variables.
}

func is_nil(v: MalVal) -> Bool       { return v.type == .TypeNil }
func is_true(v: MalVal) -> Bool      { return v.type == .TypeTrue }
func is_false(v: MalVal) -> Bool     { return v.type == .TypeFalse }
func is_integer(v: MalVal) -> Bool   { return v.type == .TypeInteger }
func is_float(v: MalVal) -> Bool     { return v.type == .TypeFloat }
func is_symbol(v: MalVal) -> Bool    { return v.type == .TypeSymbol }
func is_keyword(v: MalVal) -> Bool   { return v.type == .TypeKeyword }
func is_string(v: MalVal) -> Bool    { return v.type == .TypeString }
func is_list(v: MalVal) -> Bool      { return v.type == .TypeList }
func is_vector(v: MalVal) -> Bool    { return v.type == .TypeVector }
func is_hashmap(v: MalVal) -> Bool   { return v.type == .TypeHashMap }
func is_atom(v: MalVal) -> Bool      { return v.type == .TypeAtom }
func is_closure(v: MalVal) -> Bool   { return v.type == .TypeClosure }
func is_builtin(v: MalVal) -> Bool   { return v.type == .TypeBuiltin }
func is_comment(v: MalVal) -> Bool   { return v.type == .TypeComment }
func is_error(v: MalVal) -> Bool     { return v.type == .TypeError }

func is_truthy(v: MalVal) -> Bool    { return !is_falsey(v) }
func is_falsey(v: MalVal) -> Bool    { let type = v.type; return type == .TypeNil || type == .TypeFalse }
func is_number(v: MalVal) -> Bool    { let type = v.type; return type == .TypeInteger || type == .TypeFloat }
func is_sequence(v: MalVal) -> Bool  { let type = v.type; return type == .TypeList || type == .TypeVector }
func is_function(v: MalVal) -> Bool  { let type = v.type; return type == .TypeClosure || type == .TypeBuiltin }

func == (left: MalVal, right: MalVal) -> Bool {
    // Special case lists/vectors, since they are different types that are
    // nonetheless comparable.
    if is_sequence(left) && is_sequence(right) {
        let left_seq = left as! MalSequence
        let right_seq = right as! MalSequence
        return left_seq == right_seq
    }

    if left.type != right.type {
        return false
    }

    switch left.type {
        case .TypeUnknown:  return false
        case .TypeNil:      return (left as! MalNil) == (right as! MalNil)
        case .TypeTrue:     return (left as! MalTrue) == (right as! MalTrue)
        case .TypeFalse:    return (left as! MalFalse) == (right as! MalFalse)
        case .TypeInteger:  return (left as! MalInteger) == (right as! MalInteger)
        case .TypeFloat:    return (left as! MalFloat) == (right as! MalFloat)
        case .TypeSymbol:   return (left as! MalSymbol) == (right as! MalSymbol)
        case .TypeKeyword:  return (left as! MalKeyword) == (right as! MalKeyword)
        case .TypeString:   return (left as! MalString) == (right as! MalString)
        case .TypeList:     return (left as! MalList) == (right as! MalList)
        case .TypeVector:   return (left as! MalVector) == (right as! MalVector)
        case .TypeHashMap:  return (left as! MalHashMap) == (right as! MalHashMap)
        case .TypeAtom:     return (left as! MalAtom) == (right as! MalAtom)
        case .TypeClosure:  return (left as! MalClosure) == (right as! MalClosure)
        case .TypeBuiltin:  return (left as! MalBuiltin) == (right as! MalBuiltin)
        case .TypeComment:  return (left as! MalComment) == (right as! MalComment)
        case .TypeError:    return (left as! MalError) == (right as! MalError)
    }
}

func != (left: MalVal, right: MalVal) -> Bool {
    return !(left == right)
}

// ===== MalNil =====

class MalNil: MalVal {
    override func clone() -> MalVal { return MalNil(other:self) }
    override var type: MalType { return .TypeNil }
    override var description: String { return "nil" }
}
func == (left: MalNil, right: MalNil) -> Bool { return true }

// ===== MalTrue =====

class MalTrue: MalVal {
    override func clone() -> MalVal { return MalTrue(other:self) }
    override var type: MalType { return .TypeTrue }
    override var description: String { return "true" }
}
func == (left: MalTrue, right: MalTrue) -> Bool { return true }

// ===== MalFalse =====

class MalFalse: MalVal {
    override func clone() -> MalVal { return MalFalse(other:self) }
    override var type: MalType { return .TypeFalse }
    override var description: String { return "false" }
}
func == (left: MalFalse, right: MalFalse) -> Bool { return true }

// ===== MalInteger =====

class MalInteger: MalVal {
    init(value: Int64, meta: MalVal? = nil) { self.value = value; super.init(meta: meta) }
    init(other: MalInteger) { self.value = other.value; super.init(other: other) }
    override func clone() -> MalVal { return MalInteger(other:self) }
    override var type: MalType { return .TypeInteger }
    override var description: String { return "\(value)" }
    let value: Int64
}
func == (left: MalInteger, right: MalInteger) -> Bool { return left.value == right.value }

// ===== MalFloat =====

class MalFloat: MalVal {
    init(value: Double, meta: MalVal? = nil) { self.value = value; super.init(meta: meta) }
    init(other: MalFloat) { self.value = other.value; super.init(other: other) }
    override func clone() -> MalVal { return MalFloat(other:self) }
    override var type: MalType { return .TypeFloat }
    override var description: String { return "\(value)" }
    let value: Double
}
func == (left: MalFloat, right: MalFloat) -> Bool { return left.value == right.value }

// ===== MalSymbol =====

var symbolHash = [String:Int]()
var symbolArray = [String]()

private func indexForSymbol(s: String) -> Int {
    if let i = symbolHash[s] {
        return i
    }

    symbolArray.append(s)
    symbolHash[s] = symbolArray.count - 1
    return symbolArray.count - 1
}

private func symbolForIndex(i: Int) -> String {
    return symbolArray[i]
}

class MalSymbol: MalVal, Hashable {
    init(symbol: String, meta: MalVal? = nil) { self.index = indexForSymbol(symbol); super.init(meta: meta) }
    init(other: MalSymbol) { self.index = other.index; super.init(other: other) }
    override func clone() -> MalVal { return MalSymbol(other:self) }
    override var type: MalType { return .TypeSymbol }
    override var description: String { return symbolForIndex(self.index) }
    override var hashValue: Int { return self.index }
    let index: Int
}
func == (left: MalSymbol, right: MalSymbol) -> Bool { return left.index == right.index }

// ===== MalKeyword =====

class MalKeyword: MalVal {
    typealias Keyword = String
    init(keyword: Keyword, meta: MalVal? = nil) { self.value = keyword; super.init(meta: meta) }
    init(other: MalKeyword) { self.value = other.value; super.init(other: other) }
    override func clone() -> MalVal { return MalKeyword(other:self) }
    override var type: MalType { return .TypeKeyword }
    override var description: String { return ":\(value)" }
    let value: Keyword
}
func == (left: MalKeyword, right: MalKeyword) -> Bool { return left.value == right.value }

// ===== MalString =====

class MalString: MalVal {
    init(escaped value: String, meta: MalVal? = nil) { self.value = MalString.unescape(value); super.init(meta: meta) }   // String is quoted, and special chars are escaped with \.
    init(unescaped value: String, meta: MalVal? = nil) { self.value = value; super.init(meta: meta) }                     // String is just the way we like it.
    init(other: MalString) { self.value = other.value; super.init(other: other) }
    override func clone() -> MalVal { return MalString(other:self) }
    override var type: MalType { return .TypeString }
    override var description: String { return MalValPrintReadably ? MalString.escape(value) : value }
    let value: String

    class func unescape(s: String) -> String {
        var index = 0
        var prev_is_escape = false
        var str = ""
        for ch in s {
            if index == count(s.utf16) - 1 { continue }
            if index++ == 0 { continue }
            if prev_is_escape {
                prev_is_escape = false;
                if ch == "n" { str += "\n" }
                else if ch == "r" { str += "\r" }
                else if ch == "t" { str += "\t" }
                else { str.append(ch) }
            } else if ch == "\\" {
                prev_is_escape = true
            } else {
                str.append(ch)
            }
        }
        return str
    }

    class func escape(s: String) -> String {
        var str = ""
        for ch in s {
            if ch == "\n" { str += "\\n"; continue }
            if ch == "\r" { str += "\\r"; continue }
            if ch == "\t" { str += "\\t"; continue }
            if ch == "\"" || ch == "\\" { str += "\\" }
            str.append(ch)
        }
        str = "\"" + str + "\""
        return str
    }
}
func == (left: MalString, right: MalString) -> Bool { return left.value == right.value }

// ===== MalSequence =====

class MalSequence: MalVal, SequenceType {
    init(slice: ArraySlice<MalVal>, meta: MalVal? = nil) { self.slice = slice; super.init(meta: meta) }
    init(other: MalSequence) { self.slice = other.slice; super.init(other: other) }
    override var type: MalType { return .TypeUnknown }
    override var description: String { return "" }

    func first() -> MalVal { return !isEmpty ? slice[0] : MalNil() }
    func last() -> MalVal { return !isEmpty ? slice[slice.count - 1] : MalNil() }
    func rest() -> MalSequence { return MalSequence(slice: ArraySlice<MalVal>()) }
    func map<U>(transform: (MalVal) -> U) -> ArraySlice<U> { return slice.map(transform) }
    func reduce<U>(initial: U, combine: (U, MalVal) -> U) -> U { return slice.reduce(initial, combine: combine) }
    var count: Int { return slice.count }
    var isEmpty: Bool { return slice.isEmpty }
    subscript(index: Int) -> MalVal { return index < slice.count ? slice[index] : MalError(message: "index (\(index)) out of range (\(slice.count))") }
    subscript(subRange: Range<Int>) -> ArraySlice<MalVal> { return slice[subRange] }

    // SequenceType
     func generate() -> ArraySlice<MalVal>.Generator { return slice.generate() }

    let slice: ArraySlice<MalVal>
}
func == (left: MalSequence, right: MalSequence) -> Bool {
    if left.count != right.count { return false }
    for var index = 0; index < left.count; ++index { if left[index] != right[index] { return false } }
    return true
}

// ===== MalList =====

class MalList: MalSequence {
    override init(slice: ArraySlice<MalVal>, meta: MalVal? = nil) { super.init(slice: slice, meta: meta) }
    convenience init(array: [MalVal], meta: MalVal? = nil) { self.init(slice: array[0..<array.count], meta: meta) }
    convenience init(objects: MalVal...) { self.init(array: objects) }
    init(other: MalList) { super.init(other: other) }
    override func clone() -> MalVal { return MalList(other:self) }
    override var type: MalType { return .TypeList }
    override var description: String { return "(" + " ".join(map { $0.description }) + ")" }
    override func rest() -> MalSequence { return !isEmpty ? MalList(slice: slice[1..<slice.count]) : MalList() }
}
func == (left: MalList, right: MalList) -> Bool {
    return (left as MalSequence) == (right as MalSequence)
}

// ===== MalVector =====

class MalVector: MalSequence {
    override init(slice: ArraySlice<MalVal>, meta: MalVal? = nil) { super.init(slice: slice, meta: meta) }
    convenience init(array: [MalVal], meta: MalVal? = nil) { self.init(slice: array[0..<array.count], meta: meta) }
    convenience init(objects: MalVal...) { self.init(array: objects) }
    init(other: MalVector) { super.init(other: other) }
    override func clone() -> MalVal { return MalVector(other:self) }
    override var type: MalType { return .TypeVector }
    override var description: String { return "[" + " ".join(map { $0.description }) + "]" }
    override func rest() -> MalSequence { return !isEmpty ? MalVector(slice: slice[1..<slice.count]) : MalVector() }
}
func == (left: MalVector, right: MalVector) -> Bool {
    return (left as MalSequence) == (right as MalSequence)
}

// ===== MalHashMap =====

class MalHashMap: MalVal, SequenceType {
    convenience override init(meta: MalVal? = nil) { self.init(hash: [MalVal:MalVal](), meta:meta) }
    init(hash: [MalVal:MalVal], meta: MalVal? = nil) { self.hash = hash; super.init(meta: meta) }
    convenience init(slice: ArraySlice<MalVal>, meta: MalVal? = nil) { var hash = [MalVal:MalVal](); for var index = 0; index < slice.count; index += 2 { hash[slice[index]] = slice[index + 1] }; self.init(hash: hash, meta: meta) }
    convenience init(array: Array<MalVal>, meta: MalVal? = nil) { var hash = [MalVal:MalVal](); for var index = 0; index < array.count; index += 2 { hash[array[index]] = array[index + 1] }; self.init(hash: hash, meta: meta) }
    init(other: MalHashMap) { self.hash = other.hash; super.init(other: other) }
    override func clone() -> MalVal { return MalHashMap(other:self) }
    override var type: MalType { return .TypeHashMap }
    override var description: String {
        var a = [String]()
        for (k, v) in hash {
            a.append("\(k.description) \(v.description)")
        }
        let s = " ".join(a)
        return "{\(s)}" }
    func generate() -> DictionaryGenerator<MalVal, MalVal> { return hash.generate() }
    var count: Int { return hash.count }
    var isEmpty: Bool { return hash.isEmpty }
    subscript(key: MalVal) -> MalVal? { return hash[key] }
    let hash: [MalVal:MalVal]
}
func == (left: MalHashMap, right: MalHashMap) -> Bool {
    if left.count != right.count { return false }
    var left_index = left.hash.startIndex
    var right_index = right.hash.startIndex
    while left_index != left.hash.endIndex {
        let (left_key, left_value) = left.hash[left_index]
        let (right_key, right_value) = right.hash[right_index]
        if (left_key != right_key) || (left_value != right_value) {
            return false
        }
        left_index = left_index.successor()
        right_index = right_index.successor()
    }
    return true
}

// ===== MalAtom =====

class MalAtom: MalVal {
    init(object: MalVal, meta: MalVal? = nil) { self.value = object; super.init(meta: meta) }
    init(other: MalAtom) { self.value = other.value; super.init(other: other) }
    override func clone() -> MalVal { return MalAtom(other:self) }
    override var type: MalType { return .TypeAtom }
    override var description: String { return "(atom \(value.description))" }
    var value: MalVal
}
func == (left: MalAtom, right: MalAtom) -> Bool { return false }

// ===== MalFunction =====

class MalFunction: MalVal {
    override init(meta: MalVal? = nil) { super.init(meta: meta) }
    init(other: MalFunction) { super.init(other: other) }
    func apply(exprs: MalSequence) -> MalVal { return MalNil() }
}

// ===== MalClosure =====

class MalClosure: MalFunction {
    typealias Evaluator = (MalVal, Environment) -> MalVal
    init(eval: Evaluator, args: MalSequence, body: MalVal, env: Environment, meta: MalVal? = nil) {
        self.eval = eval;
        self.args = args;
        self.body = body;
        self.env = env;
        self.is_macro = false;
        super.init(meta: meta) }
    init(eval: Evaluator, args: MalSequence, body: MalVal, env: Environment, is_macro: Bool, meta: MalVal? = nil) {
        self.eval = eval;
        self.args = args;
        self.body = body;
        self.env = env;
        self.is_macro = is_macro;
        super.init(meta: meta) }
    init(other: MalClosure) {
        self.eval = other.eval
        self.args = other.args
        self.body = other.body
        self.env = other.env
        self.is_macro = other.is_macro
        super.init(other: other) }
    override func clone() -> MalVal { return MalClosure(other:self) }
    override var type: MalType { return .TypeClosure }
    override var description: String { return "#<Closure>: (fn* \(args.description) \(body.description))" }
    override func apply(exprs: MalSequence) -> MalVal {
        var new_env = Environment(outer: env)
        let result = new_env.set_bindings(args, with_exprs:exprs)
        if is_error(result) {
            return result
        }
        // Calling EVAL indirectly via an 'eval' data member is a bit of a hack.
        // We can't call EVAL directly because this file (types.swift) needs to
        // be used with many different versions of the main MAL file
        // (step[0-10]*.swift), and EVAL is declared differently across those
        // versions. By using this indirection, we avoid that problem.
        return eval(body, new_env)
    }
    let eval: Evaluator
    let args: MalSequence
    let body: MalVal
    let env: Environment
    var is_macro: Bool
}
func == (left: MalClosure, right: MalClosure) -> Bool { return false }

// ===== MalBuiltin =====

class MalBuiltin: MalFunction {
    typealias BuiltinSignature = (MalSequence) -> MalVal
    init(function: BuiltinSignature, meta: MalVal? = nil) { self.value = function; super.init(meta: meta) }
    init(other: MalBuiltin) { self.value = other.value; super.init(other: other) }
    override func clone() -> MalVal { return MalBuiltin(other:self) }
    override var type: MalType { return .TypeBuiltin }
    override var description: String { return "#<Builtin>" }
    override func apply(exprs: MalSequence) -> MalVal { return value(exprs) }
    let value: BuiltinSignature!
}
func == (left: MalBuiltin, right: MalBuiltin) -> Bool { return false }  // Can't compare function references in Swift

// ===== MalComment =====

class MalComment: MalVal {
    init(comment: String, meta: MalVal? = nil) { self.value = comment; super.init(meta: meta) }
    init(other: MalComment) { self.value = other.value; super.init(other: other) }
    override func clone() -> MalVal { return MalComment(other:self) }
    override var type: MalType { return .TypeComment }
    override var description: String { return value }
    let value: String
}
func == (left: MalComment, right: MalComment) -> Bool { return false }

// ===== MalError =====

class MalError: MalVal {
    init(message: String, meta: MalVal? = nil) { self.value = MalString(unescaped:message); super.init(meta: meta) }
    init(object: MalVal, meta: MalVal? = nil) { self.value = object; super.init(meta: meta) }
    init(other: MalError) { self.value = other.value; super.init(other: other) }
    override func clone() -> MalVal { return MalError(other:self) }
    override var type: MalType { return .TypeError }
    override var description: String { return value.description }
    let value: MalVal
}
func == (left: MalError, right: MalError) -> Bool { return false }
