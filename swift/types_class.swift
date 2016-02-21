//******************************************************************************
// MAL - types, implemented as a Swift "class".
//******************************************************************************

import Foundation

// ==================== Types / Constants / Variables ====================

typealias MalProtocol    = protocol<Equatable, CustomStringConvertible, Hashable>

typealias MalIntType     = Int64
typealias MalFloatType   = Double
typealias MalSymbolType  = String
typealias MalKeywordType = String
typealias MalStringType  = String
typealias MalVectorType  = ArraySlice<MalVal>
typealias MalHashType    = Dictionary<MalVal, MalVal>

private let kUnknown     = MalUnknown()
private let kNil         = MalNil()
private let kTrue        = MalTrue()
private let kFalse       = MalFalse()
private let kComment     = MalComment()

// ==================== MalVal ====================

class MalVal : MalProtocol {
    init() {
        self._meta = nil
    }
    init(_ other: MalVal, _ meta: MalVal?) {
        self._meta = meta
    }
    init(_ meta: MalVal?) {
        self._meta = meta
    }

    // CustomStringConvertible
    //
    var description: String { die() }

    // Hashable
    //
    var hashValue: Int { return description.hashValue }

    // MalVal
    //
    func clone_with_meta(meta: MalVal) -> MalVal { die() }
    final var meta: MalVal? { return self._meta }

    let _meta: MalVal?
}

// Equatable
//
let tMalUnknown = class_getName(MalUnknown)
let tMalNil     = class_getName(MalNil)
let tMalTrue    = class_getName(MalTrue)
let tMalFalse   = class_getName(MalFalse)
let tMalComment = class_getName(MalComment)
let tMalInteger = class_getName(MalInteger)
let tMalFloat   = class_getName(MalFloat)
let tMalSymbol  = class_getName(MalSymbol)
let tMalKeyword = class_getName(MalKeyword)
let tMalString  = class_getName(MalString)
let tMalList    = class_getName(MalList)
let tMalVector  = class_getName(MalVector)
let tMalHashMap = class_getName(MalHashMap)
let tMalAtom    = class_getName(MalAtom)
let tMalClosure = class_getName(MalClosure)
let tMalBuiltin = class_getName(MalBuiltin)
let tMalMacro   = class_getName(MalMacro)

func ==(left: MalVal, right: MalVal) -> Bool {
    let leftClass = object_getClassName(left)
    let rightClass = object_getClassName(right)

    if leftClass == tMalUnknown && rightClass == tMalUnknown    { return as_unknown(left)   == as_unknown(right) }
    if leftClass == tMalNil     && rightClass == tMalNil        { return as_nil(left)       == as_nil(right) }
    if leftClass == tMalTrue    && rightClass == tMalTrue       { return as_true(left)      == as_true(right) }
    if leftClass == tMalFalse   && rightClass == tMalFalse      { return as_false(left)     == as_false(right) }
    if leftClass == tMalComment && rightClass == tMalComment    { return as_comment(left)   == as_comment(right) }
    if leftClass == tMalInteger && rightClass == tMalInteger    { return as_integer(left)   == as_integer(right) }
    if leftClass == tMalFloat   && rightClass == tMalFloat      { return as_float(left)     == as_float(right) }
    if leftClass == tMalSymbol  && rightClass == tMalSymbol     { return as_symbol(left)    == as_symbol(right) }
    if leftClass == tMalKeyword && rightClass == tMalKeyword    { return as_keyword(left)   == as_keyword(right) }
    if leftClass == tMalString  && rightClass == tMalString     { return as_string(left)    == as_string(right) }
    //if leftClass == tMalList    && rightClass == tMalList       { return as_sequence(left)  == as_sequence(right) }
    //if leftClass == tMalVector  && rightClass == tMalVector     { return as_sequence(left)  == as_sequence(right) }
    if leftClass == tMalHashMap && rightClass == tMalHashMap    { return as_hashmap(left)   == as_hashmap(right) }
    if leftClass == tMalAtom    && rightClass == tMalAtom       { return as_atom(left)      == as_atom(right) }
    if leftClass == tMalClosure && rightClass == tMalClosure    { return as_closure(left)   == as_closure(right) }
    if leftClass == tMalBuiltin && rightClass == tMalBuiltin    { return as_builtin(left)   == as_builtin(right) }
    if leftClass == tMalMacro   && rightClass == tMalMacro      { return as_macro(left)     == as_macro(right) }
    //
    // Special case lists/vectors, since they are different types that are
    // nonetheless comparable.
    if
        (leftClass == tMalList || leftClass == tMalVector) &&
        (rightClass == tMalList || rightClass == tMalVector) {
            return as_sequence(left) == as_sequence(right)
        }

    return false
}

func !=(left: MalVal, right: MalVal) -> Bool {
    return !(left == right)
}

// ==================== MalUnknown ====================

final class MalUnknown: MalVal {
    override var description: String { return "unknown" }
    override func clone_with_meta(meta: MalVal) -> MalVal { return MalUnknown(meta) }
}
func ==(left: MalUnknown, right: MalUnknown) -> Bool { return false }

// ==================== MalNil ====================

final class MalNil: MalVal {
    override var description: String { return "nil" }
    override func clone_with_meta(meta: MalVal) -> MalVal { return MalNil(meta) }
}
func ==(left: MalNil, right: MalNil) -> Bool { return true }

// ==================== MalTrue ====================

final class MalTrue: MalVal {
    override var description: String { return "true" }
    override func clone_with_meta(meta: MalVal) -> MalVal { return MalTrue(meta) }
}
func ==(left: MalTrue, right: MalTrue) -> Bool { return true }

// ==================== MalFalse ====================

final class MalFalse: MalVal {
    override var description: String { return "false" }
    override func clone_with_meta(meta: MalVal) -> MalVal { return MalFalse(meta) }
}
func ==(left: MalFalse, right: MalFalse) -> Bool { return true }

// ==================== MalComment ====================

final class MalComment: MalVal {
    override var description: String { return "Comment" }
    override func clone_with_meta(meta: MalVal) -> MalVal { return MalComment(meta) }
}

// Equatable
//
func ==(left: MalComment, right: MalComment) -> Bool { return false }

// ==================== MalInteger ====================

final class MalInteger: MalVal {
    override init() {
        self._integer = 0
        super.init()
    }
    init(_ other: MalInteger, _ meta: MalVal? = nil) {
        self._integer = other._integer
        super.init(other, meta)
    }
    init(_ integer: MalIntType) {
        self._integer = integer
        super.init()
    }

    // CustomStringConvertible
    //
    override var description: String { return "\(self._integer)" }

    // Hashable
    //
    override var hashValue: Int { return Int(self._integer) }

    // MalInteger
    //
    override func clone_with_meta(meta: MalVal) -> MalVal { return MalInteger(self, meta) }
    var integer: MalIntType { return self._integer }

    private let _integer: MalIntType
}

// Equatable
//
func ==(left: MalInteger, right: MalInteger) -> Bool { return left.integer == right.integer }

// ==================== MalFloat ====================

final class MalFloat: MalVal {
    override init() {
        self._float = 0
        super.init()
    }
    init(_ other: MalFloat, _ meta: MalVal? = nil) {
        self._float = other._float
        super.init(other, meta)
    }
    init(_ float: Double) {
        self._float = float
        super.init()
    }

    // CustomStringConvertible
    //
    override var description: String { return "\(self._float)" }

    // Hashable
    //
    override var hashValue: Int { return Int(self._float) }

    // MalFloat
    //
    override func clone_with_meta(meta: MalVal) -> MalVal { return MalFloat(self, meta) }
    var float: MalFloatType { return self._float }

    private let _float: Double
}

// Equatable
//
func ==(left: MalFloat, right: MalFloat) -> Bool { return left.float == right.float }

// ==================== MalSymbol ====================

private var symbolHash = [MalSymbolType : Int]()
private var symbolArray = [MalSymbolType]()

private func indexForSymbol(s: MalSymbolType) -> Int {
    if let i = symbolHash[s] {
        return i
    }

    symbolArray.append(s)
    symbolHash[s] = symbolArray.count - 1
    return symbolArray.count - 1
}

private func symbolForIndex(i: Int) -> MalSymbolType {
    return symbolArray[i]
}

final class MalSymbol: MalVal {
    override init() {
        self._index = indexForSymbol("")
        super.init()
    }
    init(_ other: MalSymbol, _ meta: MalVal? = nil) {
        self._index = other._index
        super.init(other, meta)
    }
    init(_ symbol: MalSymbolType) {
        self._index = indexForSymbol(symbol)
        super.init()
    }

    // CustomStringConvertible
    //
    override var description: String { return symbolForIndex(self._index) }

    // Hashable
    //
    override var hashValue: Int { return self._index }

    // MalSymbol
    override func clone_with_meta(meta: MalVal) -> MalVal { return MalSymbol(self, meta) }
    var index: Int { return self._index }

    private let _index: Int
}

// Equatable
//
func ==(left: MalSymbol, right: MalSymbol) -> Bool { return left.index == right.index }

// ==================== MalKeyword ====================

final class MalKeyword: MalVal {
    override init() {
        self._keyword = ""
        super.init()
    }
    init(_ other: MalKeyword, _ meta: MalVal? = nil) {
        self._keyword = other._keyword
        super.init(other, meta)
    }
    init(_ keyword: MalKeywordType) {
        self._keyword = keyword
        super.init()
    }
    init(_ string: MalString) {
        self._keyword = string.string
        super.init()
    }

    // CustomStringConvertible
    //
    override var description: String { return self._keyword }   // ":" added in pr_str

    // MalKeyword
    //
    override func clone_with_meta(meta: MalVal) -> MalVal { return MalKeyword(self, meta) }
    var keyword: MalKeywordType { return self._keyword }

    private let _keyword: MalKeywordType
}

// Equatable
//
func ==(left: MalKeyword, right: MalKeyword) -> Bool { return left._keyword == right._keyword }

// ==================== MalString ====================

final class MalString: MalVal {
    override init() {
        self._string = ""
        super.init()
    }
    init(_ other: MalString, _ meta: MalVal? = nil) {
        self._string = other._string
        super.init(other, meta)
    }
    init(_ string: MalStringType) {
        self._string = string
        super.init()
    }

    // CustomStringConvertible
    //
    override var description: String { return self._string }

    // MalString
    //
    override func clone_with_meta(meta: MalVal) -> MalVal { return MalString(self, meta) }
    var string: MalStringType { return self._string }

    private let _string: MalStringType
}

// Equatable
//
func ==(left: MalString, right: MalString) -> Bool { return left.string == right.string }

// ==================== MalSequence ====================

class MalSequence: MalVal, SequenceType {
    override init() {
        self.count = 0
        self.isEmpty = true
        super.init()
    }
    init(_ other: MalSequence, _ meta: MalVal? = nil) {
        self.count = other.count
        self.isEmpty = other.isEmpty
        super.init(other, meta)
    }
    init(_ count: MalIntType, _ isEmpty: Bool) {
        self.count = count
        self.isEmpty = isEmpty
        super.init()
    }

    // SequenceType
    //
    func generate() -> MalVectorType.Generator { die() }

    // MalSequence
    //
    var count: MalIntType
    var isEmpty: Bool

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

final class MalList: MalSequence {
    override init() {
        self._slice = MalVectorType()
        super.init(MalIntType(self._slice.count), self._slice.isEmpty)
    }
    init(_ other: MalList, _ meta: MalVal? = nil) {
        self._slice = other._slice
        super.init(other, meta)
    }
    init(seq: MalSequence) {    // We need the "seq" in order to differentiate it from the previous init()
        self._slice = seq.reduce(MalVectorType()){ var s = $0; s.append($1); return s }
        super.init(MalIntType(self._slice.count), self._slice.isEmpty)
    }
    init(_ slice: MalVectorType) {
        self._slice = slice
        super.init(MalIntType(self._slice.count), self._slice.isEmpty)
    }
    init(_ array: Array<MalVal>) {
        self._slice = array[0..<array.count]
        super.init(MalIntType(self._slice.count), self._slice.isEmpty)
    }
    init<T: CollectionType where T.Generator.Element == MalVal>(_ collection: T) {
        self._slice = collection.reduce(MalVectorType()){ var s = $0; s.append($1); return s }
        super.init(MalIntType(self._slice.count), self._slice.isEmpty)
    }

    // CustomStringConvertible
    //
    override var description: String { return "(" + self.map { pr_str($0) }.joinWithSeparator(" ") + ")" }

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
    override func clone_with_meta(meta: MalVal) -> MalVal { return MalList(self, meta) }

    private let _slice: MalVectorType
}

// Equatable
//
func ==(left: MalList, right: MalList) -> Bool {
    return as_sequence(left) == as_sequence(right)
}

// ==================== MalVector ====================

final class MalVector: MalSequence {
    override init() {
        self._slice = MalVectorType()
        super.init(MalIntType(self._slice.count), self._slice.isEmpty)
    }
    init(_ other: MalVector, _ meta: MalVal? = nil) {
        self._slice = other._slice
        super.init(other, meta)
    }
    init(seq: MalSequence) {    // We need the "seq" in order to differentiate it from the previous init()
        self._slice = seq.reduce(MalVectorType()){ var s = $0; s.append($1); return s }
        super.init(MalIntType(self._slice.count), self._slice.isEmpty)
    }
    init(_ slice: MalVectorType) {
        self._slice = slice
        super.init(MalIntType(self._slice.count), self._slice.isEmpty)
    }
    init(_ array: Array<MalVal>) {
        self._slice = array[0..<array.count]
        super.init(MalIntType(self._slice.count), self._slice.isEmpty)
    }
    init<T: CollectionType where T.Generator.Element == MalVal>(_ collection: T) {
        self._slice = collection.reduce(MalVectorType()){ var s = $0; s.append($1); return s }
        super.init(MalIntType(self._slice.count), self._slice.isEmpty)
    }

    // CustomStringConvertible
    //
    override var description: String { return "[" + self.map { pr_str($0) }.joinWithSeparator(" ") + "]" }

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
        return make_list(result)
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
    override func clone_with_meta(meta: MalVal) -> MalVal { return MalVector(self, meta) }

    private let _slice: MalVectorType
}

// Equatable
//
func ==(left: MalVector, right: MalVector) -> Bool {
    return as_sequence(left) == as_sequence(right)
}

// ==================== MalHashMap ====================

final class MalHashMap: MalVal, SequenceType {
    override init() {
        self._hash = MalHashType()
        self.count = MalIntType(self._hash.count)
        self.isEmpty = self._hash.isEmpty
        super.init()
    }
    init(_ other: MalHashMap, _ meta: MalVal? = nil) {
        self._hash = other._hash
        self.count = MalIntType(self._hash.count)
        self.isEmpty = self._hash.isEmpty
        super.init(other, meta)
    }
    init(_ hash: MalHashType) {
        self._hash = hash
        self.count = MalIntType(self._hash.count)
        self.isEmpty = self._hash.isEmpty
        super.init()
    }
    convenience init(_ seq: MalSequence) {
        var hash = MalHashType()
        for var index: MalIntType = 0; index < seq.count; index += 2 {
            hash[try! seq.nth(index)] = try! seq.nth(index + 1)
        }
        self.init(hash)
    }
    convenience init<T: CollectionType where T.Generator.Element == MalVal>(_ collection: T) {
        // TBD: Use SequenceType/generate
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
    override var description: String {
        // TBD: Use reduce
        var a = [String]()
        for (k, v) in self._hash {
            a.append("\(pr_str(k)) \(pr_str(v))")
        }
        let s = a.joinWithSeparator(" ")
        return "{\(s)}"
    }

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

    override func clone_with_meta(meta: MalVal) -> MalVal { return MalHashMap(self, meta) }

    func value_for(key: MalVal) -> MalVal? {
        return self._hash[key]
    }

    private let _hash: MalHashType
}

// Equatable
//
func ==(left: MalHashMap, right: MalHashMap) -> Bool {
    if left.count != right.count { return false }
    var left_gen = left.generate()
    var right_gen = right.generate()
    while true {
        if let left = left_gen.next(), let right = right_gen.next() {
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

final class MalAtom: MalVal {
    override init() {
        self._object = make_nil()
        super.init()
    }
    init(_ other: MalAtom, _ meta: MalVal? = nil) {
        self._object = other._object
        super.init(other, meta)
    }
    init(object: MalVal) {
        self._object = object
        super.init()
    }

    // CustomStringConvertible
    //
    override var description: String { return "(atom \(self._object.description))" }

    // MalAtom
    //
    override func clone_with_meta(meta: MalVal) -> MalVal { return MalAtom(self, meta) }
    var object: MalVal { return self._object }

    func set_object(obj: MalVal) -> MalVal {
        self._object = obj
        return obj
    }

    private var _object: MalVal
}

// Equatable
//
func ==(left: MalAtom, right: MalAtom) -> Bool { return left.object == right.object }

// ==================== MalFunction ====================

class MalFunction: MalVal {
    override init() {
        super.init()
    }
    init(_ other: MalFunction, _ meta: MalVal? = nil) {
        super.init(other, meta)
    }

    // MalFunction
    //
    func apply(exprs: MalSequence) throws -> MalVal { die() }
}

// ==================== MalClosure ====================

final class MalClosure: MalFunction {
    typealias Evaluator = (MalVal, Environment) throws -> MalVal
    typealias Parameters = (eval: Evaluator, args: MalSequence, body: MalVal, env: Environment)

    override init() {
        self._eval = nil
        self._args = as_sequence(make_list())
        self._body = make_nil()
        self._env = Environment(outer: nil)
        super.init()
    }
    init(_ other: MalClosure, _ meta: MalVal? = nil) {
        self._eval = other._eval
        self._args = other._args
        self._body = other._body
        self._env = other._env
        super.init(other, meta)
    }
    init(_ p: Parameters) {
        self._eval = p.eval
        self._args = p.args
        self._body = p.body
        self._env = p.env
        super.init()
    }

    // CustomStringConvertible
    //
    override var description: String { return "#<Closure>: (fn* \(self._args.description) \(self._body.description))" }

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

    // MalClosure
    //
    override func clone_with_meta(meta: MalVal) -> MalVal { return MalClosure(self, meta) }

    var args: MalSequence { return self._args }
    var body: MalVal      { return self._body }
    var env:  Environment { return self._env }

    private let _eval: Evaluator!
    private let _args: MalSequence
    private let _body: MalVal
    private let _env: Environment
}

// Equatable
//
func ==(left: MalClosure, right: MalClosure) -> Bool { return false }

// ==================== MalBuiltin ====================

final class MalBuiltin: MalFunction {
    typealias Signature = (MalSequence) throws -> MalVal

    override init() {
        self._fn = nil
        super.init()
    }
    init(_ other: MalBuiltin, _ meta: MalVal? = nil) {
        self._fn = other._fn
        super.init(other, meta)
    }
    init(_ fn: Signature) {
        self._fn = fn
        super.init()
    }

    // CustomStringConvertible
    //
    override var description: String { return "#<Builtin>" }

    // MalFunction
    //
    override func apply(exprs: MalSequence) throws -> MalVal { return try self._fn(exprs) }

    // MalBuiltin
    //
    override func clone_with_meta(meta: MalVal) -> MalVal { return MalBuiltin(self, meta) }

    private let _fn: Signature!
}

// Equatable
//
func ==(left: MalBuiltin, right: MalBuiltin) -> Bool { return false }  // Can't compare function references in Swift

// ==================== MalMacro ====================

final class MalMacro : MalVal {
    override init() {
        self._closure = as_closure(make_closure())
        super.init()
    }
    init(_ other: MalMacro, _ meta: MalVal? = nil) {
        self._closure = other._closure
        super.init(other, meta)
    }
    init(_ closure: MalClosure) {
        self._closure = closure
        super.init()
    }

    // CustomStringConvertible
    //
    override var description: String { return self._closure.description }

    // MalMacro
    //
    override func clone_with_meta(meta: MalVal) -> MalVal { return MalMacro(self, meta) }

    var args: MalSequence { return self._closure.args }
    var body: MalVal      { return self._closure.body }
    var env:  Environment { return self._closure.env }

    private let _closure: MalClosure
}

// Equatable
//
func ==(left: MalMacro, right: MalMacro) -> Bool { return false }


// ==================== Constructors ====================

// ----- Default -----

func make_unknown   ()                              -> MalVal { return kUnknown }
func make_nil       ()                              -> MalVal { return kNil }
func make_true      ()                              -> MalVal { return kTrue }
func make_false     ()                              -> MalVal { return kFalse }
func make_comment   ()                              -> MalVal { return kComment }
func make_integer   ()                              -> MalVal { return MalInteger() }
func make_float     ()                              -> MalVal { return MalFloat() }
func make_symbol    ()                              -> MalVal { return MalSymbol() }
func make_keyword   ()                              -> MalVal { return MalKeyword() }
func make_string    ()                              -> MalVal { return MalString() }
func make_list      ()                              -> MalVal { return MalList() }
func make_vector    ()                              -> MalVal { return MalVector() }
func make_hashmap   ()                              -> MalVal { return MalHashMap() }
func make_atom      ()                              -> MalVal { return MalAtom() }
func make_closure   ()                              -> MalVal { return MalClosure() }
func make_builtin   ()                              -> MalVal { return MalBuiltin() }
func make_macro     ()                              -> MalVal { return MalMacro() }

// ----- Copy -----

func make_integer   (v: MalInteger)                 -> MalVal { return MalInteger(v) }
func make_float     (v: MalFloat)                   -> MalVal { return MalFloat(v) }
func make_symbol    (v: MalSymbol)                  -> MalVal { return MalSymbol(v) }
func make_keyword   (v: MalKeyword)                 -> MalVal { return MalKeyword(v) }
func make_string    (v: MalString)                  -> MalVal { return MalString(v) }
func make_list      (v: MalList)                    -> MalVal { return MalList(v) }
func make_vector    (v: MalVector)                  -> MalVal { return MalVector(v) }
func make_hashmap   (v: MalHashMap)                 -> MalVal { return MalHashMap(v) }
func make_atom      (v: MalAtom)                    -> MalVal { return MalAtom(v) }
func make_closure   (v: MalClosure)                 -> MalVal { return MalClosure(v) }
func make_builtin   (v: MalBuiltin)                 -> MalVal { return MalBuiltin(v) }
func make_macro     (v: MalMacro)                   -> MalVal { return MalMacro(v) }

// ----- Parameterized -----

func make_integer   (v: MalIntType)                 -> MalVal { return MalInteger(v) }
func make_float     (v: MalFloatType)               -> MalVal { return MalFloat(v) }
func make_symbol    (v: String)                     -> MalVal { return MalSymbol(v) }
func make_keyword   (v: String)                     -> MalVal { return MalKeyword(v) }
func make_keyword   (v: MalString)                  -> MalVal { return MalKeyword(v) }
func make_string    (v: String)                     -> MalVal { return MalString(v) }
func make_list      (v: MalSequence)                -> MalVal { return MalList(seq: v) }
func make_list      (v: MalVectorType)              -> MalVal { return MalList(v) }
func make_list      (v: Array<MalVal>)              -> MalVal { return MalList(v) }
func make_list_from (v: MalVal...)                  -> MalVal { return MalList(v) }
func make_list<T: CollectionType where T.Generator.Element == MalVal>
                    (v: T)                          -> MalVal { return MalList(v) }
func make_vector    (v: MalSequence)                -> MalVal { return MalVector(seq: v) }
func make_vector    (v: MalVectorType)              -> MalVal { return MalVector(v) }
func make_vector    (v: Array<MalVal>)              -> MalVal { return MalVector(v) }
func make_vector<T: CollectionType where T.Generator.Element == MalVal>
                    (v: T)                          -> MalVal { return MalVector(v) }
func make_hashmap   (v: MalSequence)                -> MalVal { return MalHashMap(v) }
func make_hashmap   (v: MalHashType)                -> MalVal { return MalHashMap(v) }
func make_hashmap<T: CollectionType where T.Generator.Element == MalVal>
                    (v: T)                          -> MalVal { return MalHashMap(v) }
func make_atom      (v: MalVal)                     -> MalVal { return MalAtom(object: v) }
func make_closure   (v: MalClosure.Parameters)      -> MalVal { return MalClosure(v) }
func make_builtin   (v: MalBuiltin.Signature)       -> MalVal { return MalBuiltin(v) }
func make_macro     (v: MalClosure)                 -> MalVal { return MalMacro(v) }

// ==================== Predicates ====================

// ----- Simple -----

func is_unknown     (v: MalVal) -> Bool           { return v is MalUnknown }
func is_nil         (v: MalVal) -> Bool           { return v is MalNil }
func is_true        (v: MalVal) -> Bool           { return v is MalTrue }
func is_false       (v: MalVal) -> Bool           { return v is MalFalse }
func is_comment     (v: MalVal) -> Bool           { return v is MalComment }
func is_integer     (v: MalVal) -> Bool           { return v is MalInteger }
func is_float       (v: MalVal) -> Bool           { return v is MalFloat }
func is_symbol      (v: MalVal) -> Bool           { return v is MalSymbol }
func is_keyword     (v: MalVal) -> Bool           { return v is MalKeyword }
func is_string      (v: MalVal) -> Bool           { return v is MalString }
func is_list        (v: MalVal) -> Bool           { return v is MalList }
func is_vector      (v: MalVal) -> Bool           { return v is MalVector }
func is_hashmap     (v: MalVal) -> Bool           { return v is MalHashMap }
func is_atom        (v: MalVal) -> Bool           { return v is MalAtom }
func is_closure     (v: MalVal) -> Bool           { return v is MalClosure }
func is_builtin     (v: MalVal) -> Bool           { return v is MalBuiltin }
func is_macro       (v: MalVal) -> Bool           { return v is MalMacro }

// ----- Compound -----

func is_truthy      (v: MalVal) -> Bool           { return !is_falsey(v) }
func is_falsey      (v: MalVal) -> Bool           { return is_nil(v) || is_false(v) }
func is_number      (v: MalVal) -> Bool           { return is_integer(v) || is_float(v) }
func is_sequence    (v: MalVal) -> Bool           { return is_list(v) || is_vector(v) }
func is_function    (v: MalVal) -> Bool           { return is_closure(v) || is_builtin(v) }

// ==================== Converters/Extractors ====================

func as_unknown     (v: MalVal) -> MalUnknown     { return v as! MalUnknown }
func as_nil         (v: MalVal) -> MalNil         { return v as! MalNil }
func as_true        (v: MalVal) -> MalTrue        { return v as! MalTrue }
func as_false       (v: MalVal) -> MalFalse       { return v as! MalFalse }
func as_comment     (v: MalVal) -> MalComment     { return v as! MalComment }
func as_integer     (v: MalVal) -> MalInteger     { return v as! MalInteger }
func as_float       (v: MalVal) -> MalFloat       { return v as! MalFloat }
func as_symbol      (v: MalVal) -> MalSymbol      { return v as! MalSymbol }
func as_keyword     (v: MalVal) -> MalKeyword     { return v as! MalKeyword }
func as_string      (v: MalVal) -> MalString      { return v as! MalString }
func as_list        (v: MalVal) -> MalList        { return v as! MalList }
func as_vector      (v: MalVal) -> MalVector      { return v as! MalVector }
func as_hashmap     (v: MalVal) -> MalHashMap     { return v as! MalHashMap }
func as_atom        (v: MalVal) -> MalAtom        { return v as! MalAtom }
func as_closure     (v: MalVal) -> MalClosure     { return v as! MalClosure }
func as_builtin     (v: MalVal) -> MalBuiltin     { return v as! MalBuiltin }
func as_macro       (v: MalVal) -> MalMacro       { return v as! MalMacro }

func as_sequence    (v: MalVal) -> MalSequence    { return v as! MalSequence }
func as_function    (v: MalVal) -> MalFunction    { return v as! MalFunction }

func as_inttype     (v: MalVal) -> MalIntType     { return as_integer(v).integer }
func as_floattype   (v: MalVal) -> MalFloatType   { return as_float(v).float }
func as_stringtype  (v: MalVal) -> MalStringType  { return as_string(v).string }

func as_inttype     (v: MalInteger) -> MalIntType   { return v.integer }
func as_floattype   (v: MalFloat) -> MalFloatType   { return v.float }
func as_stringtype  (v: MalString) -> MalStringType { return v.string }

func as_unknownQ    (v: MalVal) -> MalUnknown?    { return v as? MalUnknown }
func as_nilQ        (v: MalVal) -> MalNil?        { return v as? MalNil }
func as_trueQ       (v: MalVal) -> MalTrue?       { return v as? MalTrue }
func as_falseQ      (v: MalVal) -> MalFalse?      { return v as? MalFalse }
func as_commentQ    (v: MalVal) -> MalComment?    { return v as? MalComment }
func as_integerQ    (v: MalVal) -> MalInteger?    { return v as? MalInteger }
func as_floatQ      (v: MalVal) -> MalFloat?      { return v as? MalFloat }
func as_symbolQ     (v: MalVal) -> MalSymbol?     { return v as? MalSymbol }
func as_keywordQ    (v: MalVal) -> MalKeyword?    { return v as? MalKeyword }
func as_stringQ     (v: MalVal) -> MalString?     { return v as? MalString }
func as_listQ       (v: MalVal) -> MalList?       { return v as? MalList }
func as_vectorQ     (v: MalVal) -> MalVector?     { return v as? MalVector }
func as_hashmapQ    (v: MalVal) -> MalHashMap?    { return v as? MalHashMap }
func as_atomQ       (v: MalVal) -> MalAtom?       { return v as? MalAtom }
func as_closureQ    (v: MalVal) -> MalClosure?    { return v as? MalClosure }
func as_builtinQ    (v: MalVal) -> MalBuiltin?    { return v as? MalBuiltin }
func as_macroQ      (v: MalVal) -> MalMacro?      { return v as? MalMacro }

func as_sequenceQ   (v: MalVal) -> MalSequence?   { return v as? MalSequence }
func as_functionQ   (v: MalVal) -> MalFunction?   { return v as? MalFunction }

func as_inttypeQ    (v: MalVal) -> MalIntType?    { return as_integerQ(v)?.integer }
func as_floattypeQ  (v: MalVal) -> MalFloatType?  { return as_floatQ(v)?.float }
func as_stringtypeQ (v: MalVal) -> MalStringType? { return as_stringQ(v)?.string }

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

@noreturn private func die() {
    preconditionFailure("Should not get here")
}

func get_meta(v: MalVal) -> MalVal? {
    return v.meta
}

func with_meta(obj: MalVal, _ meta: MalVal) -> MalVal {
    return obj.clone_with_meta(meta)
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
