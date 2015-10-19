//******************************************************************************
// MAL - types, implemented as a Swift "enum".
//******************************************************************************

import Foundation

// ===== Types / Constants / Variables =====

typealias MalProtocol  = protocol<Equatable, CustomStringConvertible, Hashable>

typealias MalIntType     = Int64
typealias MalFloatType   = Double
typealias MalSymbolType  = String
typealias MalKeywordType = String
typealias MalStringType  = String
typealias MalVectorType  = ArraySlice<MalVal>
typealias MalHashType    = Dictionary<MalVal, MalVal>

typealias MalInteger     = MalIntType
typealias MalFloat       = MalFloatType
typealias MalSymbol      = MalSymbolType
typealias MalKeyword     = MalKeywordType
typealias MalString      = MalStringType

private let kUnknown     = MalVal.TypeUnknown
private let kNil         = MalVal.TypeNil
private let kTrue        = MalVal.TypeTrue
private let kFalse       = MalVal.TypeFalse
private let kComment     = MalVal.TypeComment

// ==================== MalSequence ====================

class MalSequence : MalProtocol, SequenceType {
    init() {
        self.count = 0
        self.isEmpty = true
    }
    init(_ seq: MalSequence) {
        self.count = seq.count
        self.isEmpty = seq.isEmpty
    }
    init(_ count: MalIntType) {
        self.count = count
        self.isEmpty = self.count == 0
    }

    // CustomStringConvertible
    //
    var description: String { die() }

    // Hashable
    //
    var hashValue: Int { die() }

    // SequenceType
    //
    func generate() -> MalVectorType.Generator { die() }

    // MalSequence
    //
    let count: MalIntType
    let isEmpty: Bool

    func first() -> MalVal { die() }
    func last() -> MalVal { die() }
    func rest() -> MalVal { die() }
    func nth(n: MalIntType) throws -> MalVal { die() }
    func range_from(from: MalIntType, to: MalIntType) -> MalVal { die() }
    func cons(element: MalVal) -> MalVal { die() }
    func concat(seq: MalSequence) throws -> MalVal { die() }
    func conj(seq: MalSequence) throws -> MalVal { die() }
    func map<U>(@noescape transform: (MalVal) -> U) -> ArraySlice<U> { die() }
    func reduce<U>(initial: U, @noescape combine: (U, MalVal) -> U) -> U { die() }
}

// Equatable
//
func ==(left: MalSequence, right: MalSequence) -> Bool {
    if left.count != right.count { return false }
    var left_gen = left.generate()
    var right_gen = right.generate()
    while true {
        if let left = left_gen.next(), right = right_gen.next() {
            if left != right {
                return false
            }
        } else {
            break
        }
    }
    return true
}

// ==================== MalList ====================

final class MalList : MalSequence {
    override convenience init() {
        self.init(MalVectorType())
    }
    init(_ other: MalList, _ meta: MalVal?) {
        self._slice = other._slice
        self._meta = meta
        super.init(other)
    }
    override convenience init(_ seq: MalSequence) {
        if let list = seq as? MalList { self.init(list._slice) }
        else
        if let vector = seq as? MalVector { self.init(vector._slice) }
        else
        { self.init(seq.reduce(MalVectorType()){ var s = $0; s.append($1); return s }) }
    }
    init(_ slice: MalVectorType) {
        self._slice = slice
        self._meta = nil
        super.init(MalIntType(self._slice.count))
    }
    convenience init(_ array: Array<MalVal>) {
        self.init(array[0..<array.count])
    }
    convenience init<T: SequenceType where T.Generator.Element == MalVal>(_ collection: T) {
        self.init(collection.reduce(MalVectorType()){ var s = $0; s.append($1); return s })
    }

    // CustomStringConvertible
    //
    override var description: String { return "(" + self.map { pr_str($0) }.joinWithSeparator(" ") + ")" }

    // Hashable
    //
    override var hashValue: Int { return description.hashValue }

    // SequenceType
    //
    override func generate() -> MalVectorType.Generator { return self._slice.generate() }

    // MalSequence
    //
    override func first() -> MalVal { return isEmpty ? make_nil() : try! nth(0) }
    override func last() -> MalVal { return try! nth(count - 1) }
    override func rest() -> MalVal { return range_from(MalIntType(1), to: MalIntType(count)) }
    override func nth(n: MalIntType) throws -> MalVal { guard n < count else { try throw_error("index (\(n)) out of range (\(count))") }; return self._slice[self._slice.startIndex.advancedBy(Int(n))] }
    override func range_from(from: MalIntType, to: MalIntType) -> MalVal {
        return from <= to && to <= count
            ? make_list(self._slice[self._slice.startIndex.advancedBy(Int(from))..<self._slice.startIndex.advancedBy(Int(to))])
            : make_list()
    }
    override func cons(element: MalVal) -> MalVal {
        var result = self._slice
        result.insert(element, atIndex: result.startIndex)
        return make_list(result)
    }
    override func concat(seq: MalSequence) throws -> MalVal {
        var result = self._slice
        if let list = as_listQ(seq) {
            result.appendContentsOf(list._slice)
        } else if let vector = as_vectorQ(seq) {
            result.appendContentsOf(vector._slice)
        } else {
            try throw_error("Expected sequence, got \(seq)")
        }
        return make_list(result)
    }
    override func conj(seq: MalSequence) throws -> MalVal {
        var result: Array<MalVal>
        if let list = as_listQ(seq) {
            result = list._slice.reverse()
        } else if let vector = as_vectorQ(seq) {
            result = vector._slice.reverse()
        } else {
            try throw_error("Expected sequence, got \(seq)")
        }
        result.appendContentsOf(self._slice)
        return make_list(result)
    }
    override func map<U>(@noescape transform: (MalVal) -> U) -> ArraySlice<U> { return ArraySlice<U>(self._slice.map(transform)) }
    override func reduce<U>(initial: U, @noescape combine: (U, MalVal) -> U) -> U { return self._slice.reduce(initial, combine: combine) }
 
    // MalList
    //
    var meta: MalVal? { return self._meta }

    private let _slice: MalVectorType
    private let _meta: MalVal?
}

// Equatable
//
func ==(left: MalList, right: MalList) -> Bool {
    return (left as MalSequence) == (right as MalSequence)
}

// ==================== MalVector ====================

final class MalVector : MalSequence {
    override convenience init() {
        self.init(MalVectorType())
    }
    init(_ other: MalVector, _ meta: MalVal?) {
        self._slice = other._slice
        self._meta = meta
        super.init(other)
    }
    override convenience init(_ seq: MalSequence) {
        if let list = seq as? MalList { self.init(list._slice) }
        else
        if let vector = seq as? MalVector { self.init(vector._slice) }
        else
        { self.init(seq.reduce(MalVectorType()){ var s = $0; s.append($1); return s }) }
    }
    init(_ slice: MalVectorType) {
        self._slice = slice
        self._meta = nil
        super.init(MalIntType(self._slice.count))
    }
    convenience init(_ array: Array<MalVal>) {
        self.init(array[0..<array.count])
    }
    convenience init<T: SequenceType where T.Generator.Element == MalVal>(_ collection: T) {
        self.init(collection.reduce(MalVectorType()){ var s = $0; s.append($1); return s })
    }

    // CustomStringConvertible
    //
    override var description: String { return "[" + self.map { pr_str($0) }.joinWithSeparator(" ") + "]" }

    // Hashable
    //
    override var hashValue: Int { return description.hashValue }

    // SequenceType
    //
    override func generate() -> MalVectorType.Generator { return self._slice.generate() }

    // MalSequence
    //
    override func first() -> MalVal { return isEmpty ? make_nil() : try! nth(0) }
    override func last() -> MalVal { return try! nth(count - 1) }
    override func rest() -> MalVal { return range_from(MalIntType(1), to: MalIntType(count)) }
    override func nth(n: MalIntType) throws -> MalVal { guard n < count else { try throw_error("index (\(n)) out of range (\(count))") }; return self._slice[self._slice.startIndex.advancedBy(Int(n))] }
    override func range_from(from: MalIntType, to: MalIntType) -> MalVal {
        return from <= to && to <= count
            ? make_list(self._slice[self._slice.startIndex.advancedBy(Int(from))..<self._slice.startIndex.advancedBy(Int(to))])   // Yes, make_list
            : make_list()                                   // Yes, make_list
    }
    override func cons(element: MalVal) -> MalVal {
        var result = self._slice
        result.insert(element, atIndex: result.startIndex)
        return make_list(result)                            // Yes, make_list
    }
    override func concat(seq: MalSequence) throws -> MalVal {
        var result = self._slice
        if let list = as_listQ(seq) {
            result.appendContentsOf(list._slice)
        } else if let vector = as_vectorQ(seq) {
            result.appendContentsOf(vector._slice)
        } else {
            try throw_error("Expected sequence, got \(seq)")
        }
        return make_vector(result)
    }
    override func conj(seq: MalSequence) throws -> MalVal {
        var result = self._slice
        if let list = as_listQ(seq) {
            result.appendContentsOf(list._slice)
        } else if let vector = as_vectorQ(seq) {
            result.appendContentsOf(vector._slice)
        } else {
            try throw_error("Expected sequence, got \(seq)")
        }
        return make_vector(result)
    }
    override func map<U>(@noescape transform: (MalVal) -> U) -> ArraySlice<U> { return ArraySlice<U>(self._slice.map(transform)) }
    override func reduce<U>(initial: U, @noescape combine: (U, MalVal) -> U) -> U { return self._slice.reduce(initial, combine: combine) }

    // MalVector
    //
    var meta: MalVal? { return self._meta }

    private let _slice: MalVectorType
    private let _meta: MalVal?
}

// Equatable
//
func ==(left: MalVector, right: MalVector) -> Bool {
    return (left as MalSequence) == (right as MalSequence)
}

// ==================== MalHashMap ====================

final class MalHashMap : MalProtocol, SequenceType {
    convenience init() {
        self.init(MalHashType())
    }
    init(_ other: MalHashMap, _ meta: MalVal?) {
        self._hash = other._hash
        self._meta = meta
        self.count = MalIntType(self._hash.count)
        self.isEmpty = self._hash.isEmpty
    }
    init(_ hash: MalHashType) {
        self._hash = hash
        self._meta = nil
        self.count = MalIntType(self._hash.count)
        self.isEmpty = self._hash.isEmpty
    }
    convenience init(_ seq: MalSequence) {
        var hash = MalHashType()
        for var index: MalIntType = 0; index < seq.count; index += 2 {
            hash[try! seq.nth(index)] = try! seq.nth(index + 1)
        }
        self.init(hash)
    }
    convenience init<T: CollectionType where T.Generator.Element == MalVal>(_ collection: T) {
        var hash = MalHashType()
        for var index = collection.startIndex; index != collection.endIndex; {
            let key = collection[index++]
            let value = collection[index++]
            hash[key] = value
        }
        self.init(hash)
    }

    // CustomStringConvertible
    //
    var description: String {
        var a = [String]()
        for (k, v) in self._hash {
            a.append("\(pr_str(k)) \(pr_str(v))")
        }
        let s = a.joinWithSeparator(" ")
        return "{\(s)}"
    }

    // Hashable
    //
    var hashValue: Int { return description.hashValue }

    // SequenceType
    //
    func generate() -> MalHashType.Generator { return self._hash.generate() }

    // MalHashMap
    //
    let count: MalIntType
    let isEmpty: Bool
    var hash: MalHashType { return self._hash }
    var keys: MalVal { return make_list(self._hash.keys) }
    var values: MalVal { return make_list(self._hash.values) }
    var meta: MalVal? { return self._meta }

    func value_for(key: MalVal) -> MalVal? {
        return self._hash[key]
    }

    private let _hash: MalHashType
    private let _meta: MalVal?
}

// Equatable
//
func ==(left: MalHashMap, right: MalHashMap) -> Bool {
    if left.count != right.count { return false }
    var left_gen = left.generate()
    var right_gen = right.generate()
    while true {
        if let left = left_gen.next(), right = right_gen.next() {
            if left.0 != right.0 || left.1 != right.1 {
                return false
            }
        } else {
            break
        }
    }
    return true
}

// ==================== MalAtom ====================

final class MalAtom : MalProtocol {
    convenience init() {
        self.init(make_nil())
    }
    init(_ other: MalAtom, _ meta: MalVal?) {
        self._object = other._object
        self._meta = meta
    }
    init(_ object: MalVal) {
        self._object = object
        self._meta = nil
    }

    // CustomStringConvertible
    //
    var description: String { return "(atom \(pr_str(self._object)))" }

    // Hashable
    //
    var hashValue: Int { return description.hashValue }

    // MalAtom
    //
    var object: MalVal { return self._object }
    var meta: MalVal? { return self._meta }

    func set_object(obj: MalVal) -> MalVal {
        self._object = obj
        return obj
    }

    private var _object: MalVal
    private let _meta: MalVal?
}

// Equatable
//
func ==(left: MalAtom, right: MalAtom) -> Bool { return left.object == right.object }

// ==================== MalFunction ====================

class MalFunction : MalProtocol {
    init() {
    }
    init(_ other: MalFunction) {
    }

    // CustomStringConvertible
    //
    var description: String { die() }

    // Hashable
    //
    var hashValue: Int { die() }

    // MalFunction
    //
    func apply(exprs: MalSequence) throws -> MalVal { die() }
}

// Equatable
//
func ==(left: MalFunction, right: MalFunction) -> Bool { return false }

// ==================== MalClosure ====================


final class MalClosure : MalFunction {
    typealias Evaluator = (MalVal, Environment) throws -> MalVal
    typealias Parameters = (eval: Evaluator, args: MalSequence, body: MalVal, env: Environment)

    override convenience init() {
        self.init((
            eval: {(a: MalVal, b: Environment) -> MalVal in make_nil() },
            args: as_sequence(make_list()),
            body: make_nil(),
            env:  Environment(outer: nil)
        ))
    }
    init(_ other: MalClosure, _ meta: MalVal?) {
        self._eval = other._eval
        self._args = other._args
        self._body = other._body
        self._env = other._env
        self._meta = meta
        super.init(other)
    }
    init(_ p: Parameters) {
        self._eval = p.eval
        self._args = p.args
        self._body = p.body
        self._env = p.env
        self._meta = nil
        super.init()
    }

    // CustomStringConvertible
    //
    override var description: String { return "#<Closure>: (fn* \(self._args.description) \(self._body.description))" }

    // Hashable
    //
    override var hashValue: Int { return description.hashValue }

    // MalFunction
    //
    override func apply(exprs: MalSequence) throws -> MalVal {
        let new_env = Environment(outer: self._env)
        let _ = try new_env.set_bindings(self._args, with_exprs: exprs)
        // Calling EVAL indirectly via an 'eval' data member is a bit of a hack.
        // We can't call EVAL directly because this file (types.swift) needs to
        // be used with many different versions of the main MAL file
        // (step[0-10]*.swift), and EVAL is declared differently across those
        // versions. By using this indirection, we avoid that problem.
        return try self._eval(self._body, new_env)
    }

    var args: MalSequence { return self._args }
    var body: MalVal      { return self._body }
    var env:  Environment { return self._env }
    var meta: MalVal?     { return self._meta }

    private let _eval: Evaluator!
    private let _args: MalSequence
    private let _body: MalVal
    private let _env: Environment
    private let _meta: MalVal?
}

// Equatable
//
func ==(left: MalClosure, right: MalClosure) -> Bool { return false }

// ==================== MalBuiltin ====================

final class MalBuiltin : MalFunction {
    typealias Signature = (MalSequence) throws -> MalVal

    override convenience init() {
        self.init( {(MalSequence) -> MalVal in make_nil()} )
    }
    init(_ other: MalBuiltin, _ meta: MalVal?) {
        self._fn = other._fn
        self._meta = meta
        super.init(other)
    }
    init(_ fn: Signature) {
        self._fn = fn
        self._meta = nil
        super.init()
    }

    // CustomStringConvertible
    //
    override var description: String { return "#<Builtin>" }

    // Hashable
    //
    override var hashValue: Int { return description.hashValue }

    // MalBuiltin
    //
    override func apply(exprs: MalSequence) throws -> MalVal { return try self._fn(exprs) }
    var meta: MalVal? { return self._meta }

    private let _fn: Signature!
    private let _meta: MalVal?
}

// Equatable
//
func ==(left: MalBuiltin, right: MalBuiltin) -> Bool { return false }  // Can't compare function references in Swift

// ==================== MalMacro ====================

final class MalMacro : MalProtocol {
    convenience init() {
        self.init(as_closure(make_closure()))
    }
    init(_ other: MalMacro, _ meta: MalVal?) {
        self._closure = other._closure
        self._meta = meta
    }
    init(_ closure: MalClosure) {
        self._closure = closure
        self._meta = nil
    }

    // CustomStringConvertible
    //
    var description: String { return self._closure.description }

    // Hashable
    //
    var hashValue: Int { return description.hashValue }

    var args: MalSequence { return self._closure.args }
    var body: MalVal      { return self._closure.body }
    var env:  Environment { return self._closure.env }
    var meta: MalVal?     { return self._meta }

    private let _closure: MalClosure
    private let _meta: MalVal?
}

// Equatable
//
func ==(left: MalMacro, right: MalMacro) -> Bool { return false }

// ==================== MalVal ====================

enum MalVal : MalProtocol {
    case TypeUnknown
    case TypeNil
    case TypeTrue
    case TypeFalse
    case TypeComment
    case TypeInteger (MalInteger)
    case TypeFloat   (MalFloat)
    case TypeSymbol  (MalSymbol)
    case TypeKeyword (MalKeyword)
    case TypeString  (MalString)
    case TypeList    (MalList)
    case TypeVector  (MalVector)
    case TypeHashMap (MalHashMap)
    case TypeAtom    (MalAtom)
    case TypeClosure (MalClosure)
    case TypeBuiltin (MalBuiltin)
    case TypeMacro   (MalMacro)

    // CustomStringConvertible
    //
    var description: String {
        switch self {
            case .TypeUnknown: return "unknown"
            case .TypeNil:     return "nil"
            case .TypeTrue:    return "true"
            case .TypeFalse:   return "false"
            case .TypeComment: return "comment"
            case .TypeInteger  (let v): return v.description
            case .TypeFloat    (let v): return v.description
            case .TypeSymbol   (let v): return v
            case .TypeKeyword  (let v): return v
            case .TypeString   (let v): return v
            case .TypeList     (let v): return v.description
            case .TypeVector   (let v): return v.description
            case .TypeHashMap  (let v): return v.description
            case .TypeAtom     (let v): return v.description
            case .TypeClosure  (let v): return v.description
            case .TypeBuiltin  (let v): return v.description
            case .TypeMacro    (let v): return v.description
        }
    }

    // Hashable
    //
    var hashValue: Int {
        switch self {
            case .TypeUnknown: return 0
            case .TypeNil:     return 0
            case .TypeTrue:    return 0
            case .TypeFalse:   return 0
            case .TypeComment: return 0
            case .TypeInteger  (let v): return v.hashValue
            case .TypeFloat    (let v): return v.hashValue
            case .TypeSymbol   (let v): return v.hashValue
            case .TypeKeyword  (let v): return v.hashValue
            case .TypeString   (let v): return v.hashValue
            case .TypeList     (let v): return v.hashValue
            case .TypeVector   (let v): return v.hashValue
            case .TypeHashMap  (let v): return v.hashValue
            case .TypeAtom     (let v): return v.hashValue
            case .TypeClosure  (let v): return v.hashValue
            case .TypeBuiltin  (let v): return v.hashValue
            case .TypeMacro    (let v): return v.hashValue
        }
    }
}

// Equatable
//
func ==(left: MalVal, right: MalVal) -> Bool {
    switch (left, right) {
        case (.TypeUnknown,             .TypeUnknown):              return true
        case (.TypeNil,                 .TypeNil):                  return true
        case (.TypeTrue,                .TypeTrue):                 return true
        case (.TypeFalse,               .TypeFalse):                return true
        case (.TypeComment,             .TypeComment):              return false
        case (.TypeInteger (let vLeft), .TypeInteger (let vRight)): return vLeft == vRight
        case (.TypeFloat   (let vLeft), .TypeFloat   (let vRight)): return vLeft == vRight
        case (.TypeSymbol  (let vLeft), .TypeSymbol  (let vRight)): return vLeft == vRight
        case (.TypeKeyword (let vLeft), .TypeKeyword (let vRight)): return vLeft == vRight
        case (.TypeString  (let vLeft), .TypeString  (let vRight)): return vLeft == vRight
        case (.TypeList    (let vLeft), .TypeList    (let vRight)): return vLeft == vRight
        case (.TypeVector  (let vLeft), .TypeVector  (let vRight)): return vLeft == vRight
        case (.TypeHashMap (let vLeft), .TypeHashMap (let vRight)): return vLeft == vRight
        case (.TypeAtom    (let vLeft), .TypeAtom    (let vRight)): return vLeft == vRight
        case (.TypeClosure (let vLeft), .TypeClosure (let vRight)): return vLeft == vRight
        case (.TypeBuiltin (let vLeft), .TypeBuiltin (let vRight)): return vLeft == vRight
        case (.TypeMacro   (let vLeft), .TypeMacro   (let vRight)): return vLeft == vRight

        case (.TypeList    (let vLeft), .TypeVector  (let vRight)): return vLeft == vRight
        case (.TypeVector  (let vLeft), .TypeList    (let vRight)): return vLeft == vRight

        default:                                                    return false
    }
}

func ==(left: MalList, right: MalVector) -> Bool {
    if left.count != right.count { return false }
    var left_gen = left.generate()
    var right_gen = right.generate()
    while true {
        if let left = left_gen.next(), right = right_gen.next() {
            if left != right {
                return false
            }
        } else {
            break
        }
    }
    return true
}

func ==(left: MalVector, right: MalList) -> Bool {
    if left.count != right.count { return false }
    var left_gen = left.generate()
    var right_gen = right.generate()
    while true {
        if let left = left_gen.next(), right = right_gen.next() {
            if left != right {
                return false
            }
        } else {
            break
        }
    }
    return true
}

// ==================== Constructors ====================

// ----- Default -----

func make_unknown     ()                              -> MalVal { return kUnknown }
func make_nil         ()                              -> MalVal { return kNil }
func make_true        ()                              -> MalVal { return kTrue }
func make_false       ()                              -> MalVal { return kFalse }
func make_comment     ()                              -> MalVal { return kComment }
func make_integer     ()                              -> MalVal { return make_integer   (MalInteger()) }
func make_float       ()                              -> MalVal { return make_float     (MalFloat()) }
func make_symbol      ()                              -> MalVal { return make_symbol    (MalSymbol()) }
func make_keyword     ()                              -> MalVal { return make_keyword   (MalKeyword()) }
func make_string      ()                              -> MalVal { return make_string    (MalString()) }
func make_list        ()                              -> MalVal { return make_list      (MalList()) }
func make_vector      ()                              -> MalVal { return make_vector    (MalVector()) }
func make_hashmap     ()                              -> MalVal { return make_hashmap   (MalHashMap()) }
func make_atom        ()                              -> MalVal { return make_atom      (MalAtom()) }
func make_closure     ()                              -> MalVal { return make_closure   (MalClosure()) }
func make_builtin     ()                              -> MalVal { return make_builtin   (MalBuiltin()) }
func make_macro       ()                              -> MalVal { return make_macro     (MalMacro()) }

// ----- Base -----

func make_integer     (v: MalInteger)                 -> MalVal { return MalVal.TypeInteger(v) }
func make_float       (v: MalFloat)                   -> MalVal { return MalVal.TypeFloat(v) }
func make_symbol      (v: MalSymbol)                  -> MalVal { return MalVal.TypeSymbol(v) }
func make_keyword     (v: MalKeyword)                 -> MalVal { return MalVal.TypeKeyword(v) }
func make_string      (v: MalString)                  -> MalVal { return MalVal.TypeString(v) }
func make_list        (v: MalList)                    -> MalVal { return MalVal.TypeList(v) }
func make_vector      (v: MalVector)                  -> MalVal { return MalVal.TypeVector(v) }
func make_hashmap     (v: MalHashMap)                 -> MalVal { return MalVal.TypeHashMap(v) }
func make_atom        (v: MalAtom)                    -> MalVal { return MalVal.TypeAtom(v) }
func make_closure     (v: MalClosure)                 -> MalVal { return MalVal.TypeClosure(v) }
func make_builtin     (v: MalBuiltin)                 -> MalVal { return MalVal.TypeBuiltin(v) }
func make_macro       (v: MalMacro)                   -> MalVal { return MalVal.TypeMacro(v) }

// ----- Parameterized -----

func make_list        (v: MalSequence)                -> MalVal { return make_list(MalList(v)) }
func make_list        (v: MalVectorType)              -> MalVal { return make_list(MalList(v)) }
func make_list        (v: Array<MalVal>)              -> MalVal { return make_list(MalList(v)) }
func make_list_from   (v: MalVal...)                  -> MalVal { return make_list(MalList(v)) }
func make_list<T: SequenceType where T.Generator.Element == MalVal>
                      (v: T)                          -> MalVal { return make_list(MalList(v)) }
func make_vector      (v: MalSequence)                -> MalVal { return make_vector(MalVector(v)) }
func make_vector      (v: MalVectorType)              -> MalVal { return make_vector(MalVector(v)) }
func make_vector      (v: Array<MalVal>)              -> MalVal { return make_vector(MalVector(v)) }
func make_vector_from (v: MalVal...)                  -> MalVal { return make_vector(MalVector(v)) }
func make_vector<T: SequenceType where T.Generator.Element == MalVal>
                      (v: T)                          -> MalVal { return make_vector(MalVector(v)) }
func make_hashmap     (v: MalSequence)                -> MalVal { return make_hashmap(MalHashMap(v)) }
func make_hashmap     (v: MalHashType)                -> MalVal { return make_hashmap(MalHashMap(v)) }
func make_hashmap<T: CollectionType where T.Generator.Element == MalVal>
                      (v: T)                          -> MalVal { return make_hashmap(MalHashMap(v)) }
func make_atom        (v: MalVal)                     -> MalVal { return make_atom(MalAtom(v)) }
func make_closure     (v: MalClosure.Parameters)      -> MalVal { return make_closure(MalClosure(v)) }
func make_builtin     (v: MalBuiltin.Signature)       -> MalVal { return make_builtin(MalBuiltin(v)) }
func make_macro       (v: MalClosure)                 -> MalVal { return make_macro(MalMacro(v)) }

// ==================== Predicates ====================

// ----- Simple -----

func is_unknown       (v: MalVal) -> Bool { if case .TypeUnknown = v { return true } else { return false } }
func is_nil           (v: MalVal) -> Bool { if case .TypeNil     = v { return true } else { return false } }
func is_true          (v: MalVal) -> Bool { if case .TypeTrue    = v { return true } else { return false } }
func is_false         (v: MalVal) -> Bool { if case .TypeFalse   = v { return true } else { return false } }
func is_comment       (v: MalVal) -> Bool { if case .TypeComment = v { return true } else { return false } }
func is_integer       (v: MalVal) -> Bool { if case .TypeInteger = v { return true } else { return false } }
func is_float         (v: MalVal) -> Bool { if case .TypeFloat   = v { return true } else { return false } }
func is_symbol        (v: MalVal) -> Bool { if case .TypeSymbol  = v { return true } else { return false } }
func is_keyword       (v: MalVal) -> Bool { if case .TypeKeyword = v { return true } else { return false } }
func is_string        (v: MalVal) -> Bool { if case .TypeString  = v { return true } else { return false } }
func is_list          (v: MalVal) -> Bool { if case .TypeList    = v { return true } else { return false } }
func is_vector        (v: MalVal) -> Bool { if case .TypeVector  = v { return true } else { return false } }
func is_hashmap       (v: MalVal) -> Bool { if case .TypeHashMap = v { return true } else { return false } }
func is_atom          (v: MalVal) -> Bool { if case .TypeAtom    = v { return true } else { return false } }
func is_closure       (v: MalVal) -> Bool { if case .TypeClosure = v { return true } else { return false } }
func is_builtin       (v: MalVal) -> Bool { if case .TypeBuiltin = v { return true } else { return false } }
func is_macro         (v: MalVal) -> Bool { if case .TypeMacro   = v { return true } else { return false } }

// ----- Compound -----

func is_truthy        (v: MalVal) -> Bool { return !is_falsey(v) }
func is_falsey        (v: MalVal) -> Bool { switch v { case .TypeNil,     .TypeFalse:   return true; default: return false } }
func is_number        (v: MalVal) -> Bool { switch v { case .TypeInteger, .TypeFloat:   return true; default: return false } }
func is_sequence      (v: MalVal) -> Bool { switch v { case .TypeList,    .TypeVector:  return true; default: return false } }
func is_function      (v: MalVal) -> Bool { switch v { case .TypeClosure, .TypeBuiltin: return true; default: return false } }

// ==================== Converters/Extractors ====================

func as_integer       (v: MalVal) -> MalInteger  { if case .TypeInteger(let w) = v { return w }; die("expected integer, got \(v)") }
func as_float         (v: MalVal) -> MalFloat    { if case .TypeFloat(let w)   = v { return w }; die("expected float, got \(v)") }
func as_symbol        (v: MalVal) -> MalSymbol   { if case .TypeSymbol(let w)  = v { return w }; die("expected symbol, got \(v)") }
func as_keyword       (v: MalVal) -> MalKeyword  { if case .TypeKeyword(let w) = v { return w }; die("expected keyword, got \(v)") }
func as_string        (v: MalVal) -> MalString   { if case .TypeString(let w)  = v { return w }; die("expected string, got \(v)") }
func as_list          (v: MalVal) -> MalList     { if case .TypeList(let w)    = v { return w }; die("expected list, got \(v)") }
func as_vector        (v: MalVal) -> MalVector   { if case .TypeVector(let w)  = v { return w }; die("expected vector, got \(v)") }
func as_hashmap       (v: MalVal) -> MalHashMap  { if case .TypeHashMap(let w) = v { return w }; die("expected hashmap, got \(v)") }
func as_atom          (v: MalVal) -> MalAtom     { if case .TypeAtom(let w)    = v { return w }; die("expected atom, got \(v)") }
func as_closure       (v: MalVal) -> MalClosure  { if case .TypeClosure(let w) = v { return w }; die("expected closure, got \(v)") }
func as_builtin       (v: MalVal) -> MalBuiltin  { if case .TypeBuiltin(let w) = v { return w }; die("expected builtin, got \(v)") }
func as_macro         (v: MalVal) -> MalMacro    { if case .TypeMacro(let w)   = v { return w }; die("expected macro, got \(v)") }

func as_sequence      (v: MalVal) -> MalSequence {
    switch v {
        case .TypeList(let v): return v
        case .TypeVector(let v): return v
        default: die("expected sequence, got \(v)")
    }
}
func as_function      (v: MalVal) -> MalFunction {
    switch v {
        case .TypeClosure(let v): return v
        case .TypeBuiltin(let v): return v
        default: die("expected function, got \(v)")
    }
}

func as_inttype       (v: MalVal) -> MalIntType     { return as_integer(v) }
func as_floattype     (v: MalVal) -> MalFloatType   { return as_float(v) }
func as_stringtype    (v: MalVal) -> MalStringType  { return as_string(v) }

func as_inttype       (v: MalInteger) -> MalIntType   { return v }
func as_floattype     (v: MalFloat) -> MalFloatType   { return v }
func as_stringtype    (v: MalString) -> MalStringType { return v }

func as_integerQ      (v: MalVal) -> MalInteger? { if case .TypeInteger(let w) = v { return w }; return nil }
func as_floatQ        (v: MalVal) -> MalFloat?   { if case .TypeFloat(let w)   = v { return w }; return nil }
func as_symbolQ       (v: MalVal) -> MalSymbol?  { if case .TypeSymbol(let w)  = v { return w }; return nil }
func as_keywordQ      (v: MalVal) -> MalKeyword? { if case .TypeKeyword(let w) = v { return w }; return nil }
func as_stringQ       (v: MalVal) -> MalString?  { if case .TypeString(let w)  = v { return w }; return nil }
func as_listQ         (v: MalVal) -> MalList?    { if case .TypeList(let w)    = v { return w }; return nil }
func as_vectorQ       (v: MalVal) -> MalVector?  { if case .TypeVector(let w)  = v { return w }; return nil }
func as_hashmapQ      (v: MalVal) -> MalHashMap? { if case .TypeHashMap(let w) = v { return w }; return nil }
func as_atomQ         (v: MalVal) -> MalAtom?    { if case .TypeAtom(let w)    = v { return w }; return nil }
func as_closureQ      (v: MalVal) -> MalClosure? { if case .TypeClosure(let w) = v { return w }; return nil }
func as_builtinQ      (v: MalVal) -> MalBuiltin? { if case .TypeBuiltin(let w) = v { return w }; return nil }
func as_macroQ        (v: MalVal) -> MalMacro?   { if case .TypeMacro(let w)   = v { return w }; return nil }

func as_listQ         (v: MalSequence) -> MalList?    { return v as? MalList }
func as_vectorQ       (v: MalSequence) -> MalVector?  { return v as? MalVector }

func as_sequenceQ     (v: MalVal) -> MalSequence? {
    switch v {
        case .TypeList(let v): return v
        case .TypeVector(let v): return v
        default: return nil
    }
}
func as_functionQ     (v: MalVal) -> MalFunction? {
    switch v {
        case .TypeClosure(let v): return v
        case .TypeBuiltin(let v): return v
        default: return nil
    }
}

func as_inttypeQ      (v: MalVal) -> MalIntType?    { return as_integerQ(v) }
func as_floattypeQ    (v: MalVal) -> MalFloatType?  { return as_floatQ(v) }
func as_stringtypeQ   (v: MalVal) -> MalStringType? { return as_stringQ(v) }

// ==================== Exceptions ====================

enum MalException: ErrorType, CustomStringConvertible {
    case None
    case Message(String)
    case Object(MalVal)

    var exception: MalVal {
        switch self {
            case .None:
                return make_nil()
            case .Message(let v):
                return make_string(v)
            case .Object(let v):
                return v
        }
    }

    // CustomStringConvertible
    //
    var description: String {
        switch self {
            case .None:
                return "NIL Exception"
            case .Message(let v):
                return v
            case .Object(let v):
                return v.description
        }
    }
}

@noreturn
func throw_error(v: String) throws { throw MalException.Message(v) }

@noreturn
func throw_error(v: MalVal) throws { throw MalException.Object(v) }

// ==================== Utilities ====================

@noreturn private func die(msg: String) {
    preconditionFailure(msg)
}

@noreturn private func die() {
    die("Should not get here")
}

func get_meta(v: MalVal) -> MalVal? {
    switch v {
        case .TypeUnknown: return nil
        case .TypeNil:     return nil
        case .TypeTrue:    return nil
        case .TypeFalse:   return nil
        case .TypeComment: return nil
        case .TypeInteger: return nil
        case .TypeFloat:   return nil
        case .TypeSymbol:  return nil
        case .TypeKeyword: return nil
        case .TypeString:  return nil
        case .TypeList     (let v): return v.meta
        case .TypeVector   (let v): return v.meta
        case .TypeHashMap  (let v): return v.meta
        case .TypeAtom     (let v): return v.meta
        case .TypeClosure  (let v): return v.meta
        case .TypeBuiltin  (let v): return v.meta
        case .TypeMacro    (let v): return v.meta
    }
}

func with_meta(obj: MalVal, _ meta: MalVal) -> MalVal {
    switch obj {
        case .TypeUnknown: return obj
        case .TypeNil:     return obj
        case .TypeTrue:    return obj
        case .TypeFalse:   return obj
        case .TypeComment: return obj
        case .TypeInteger: return obj
        case .TypeFloat:   return obj
        case .TypeSymbol:  return obj
        case .TypeKeyword: return obj
        case .TypeString:  return obj
        case .TypeList     (let v): return make_list(MalList(v, meta))
        case .TypeVector   (let v): return make_vector(MalVector(v, meta))
        case .TypeHashMap  (let v): return make_hashmap(MalHashMap(v, meta))
        case .TypeAtom     (let v): return make_atom(MalAtom(v, meta))
        case .TypeClosure  (let v): return make_closure(MalClosure(v, meta))
        case .TypeBuiltin  (let v): return make_builtin(MalBuiltin(v, meta))
        case .TypeMacro    (let v): return make_macro(MalMacro(v, meta))
    }
}

func unescape(s: String) -> String {
    var index = 0
    var prev_is_escape = false
    var str = ""
    let chars = s.characters
    for ch in chars {
        if index == chars.count - 1 { continue }
        if index++ == 0 { continue }
        if prev_is_escape {
            prev_is_escape = false
            if ch == "n" { str.appendContentsOf("\n") }
            else if ch == "r" { str.appendContentsOf("\r") }
            else if ch == "t" { str.appendContentsOf("\t") }
            else { str.append(ch) }
        } else if ch == "\\" {
            prev_is_escape = true
        } else {
            str.append(ch)
        }
    }
    return str
}

func escape(s: String) -> String {
    var str = ""
    let chars = s.characters
    for ch in chars {
        if ch == "\n" { str.appendContentsOf("\\n"); continue }
        if ch == "\r" { str.appendContentsOf("\\r"); continue }
        if ch == "\t" { str.appendContentsOf("\\t"); continue }
        if ch == "\"" || ch == "\\" { str.appendContentsOf("\\") }
        str.append(ch)
    }
    str = "\"" + str + "\""
    return str
}
