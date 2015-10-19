//******************************************************************************
// MAL - core
//******************************************************************************

import Foundation

// This is a simple type distinct from all MalVal types so that we can pass a
// sequence to a function and be able to distinguish between those functions
// that want a sequence as a parameter and those that want a sequence that holds
// the rest of the function parameters.
//
final class MalVarArgs {
    init(_ value: MalSequence) { self.value = value }
    init(_ value: MalVal) { self.value = as_sequence(value) }
    let value: MalSequence
}

private func fn_eq(obj1: MalVal, obj2: MalVal) throws -> Bool {
    return obj1 == obj2
}

private func fn_throw(exception: MalVal) throws -> MalVal {
    try throw_error(exception)
}

private func fn_nilQ(obj: MalVal) throws -> Bool {
    return is_nil(obj)
}

private func fn_trueQ(obj: MalVal) throws -> Bool {
    return is_true(obj)
}

private func fn_falseQ(obj: MalVal) throws -> Bool {
    return is_false(obj)
}

private func fn_symbol(s: String) throws -> MalVal {
    return make_symbol(s)
}

private func fn_symbolQ(obj: MalVal) throws -> Bool {
    return is_symbol(obj)
}

private func fn_keyword(s: MalVal) throws -> MalVal {
    if is_keyword(s) {
        return s
    }
    if is_string(s) {
        return make_keyword(as_string(s))
    }
    try throw_error("expected string or keyword")
}

private func fn_keywordQ(obj: MalVal) throws -> Bool {
    return is_keyword(obj)
}

private func fn_prstr(args: MalVarArgs) throws -> String {
    let args_str_array = args.value.map { pr_str($0, true) }
    return args_str_array.joinWithSeparator(" ")
}

private func fn_str(args: MalVarArgs) throws -> String {
    let args_str_array = args.value.map { pr_str($0, false) }
    return args_str_array.joinWithSeparator("")
}

private func fn_prn(args: MalVarArgs) {
    let args_str_array = args.value.map { pr_str($0, true) }
    let args_str = args_str_array.joinWithSeparator(" ")
    print(args_str)
}

private func fn_println(args: MalVarArgs) {
    let args_str_array = args.value.map { pr_str($0, false) }
    let args_str = args_str_array.joinWithSeparator(" ")
    print(args_str)
}

private func fn_readstring(s: String) throws -> MalVal {
    return try read_str(s)
}

private func fn_readline(s: String) throws -> String? {
    return _readline(s)
}

private func fn_slurp(s: String) throws -> MalVal {
    do {
        let result = try String(contentsOfFile: s, encoding: NSUTF8StringEncoding)
        return make_string(result)
    } catch let error as NSError {
        try throw_error("unknown error reading file \(error)")
    }
}

private func fn_lt(arg1: MalIntType, arg2: MalIntType) throws -> Bool {
    return arg1 < arg2
}

private func fn_lte(arg1: MalIntType, arg2: MalIntType) throws -> Bool {
    return arg1 <= arg2
}

private func fn_gt(arg1: MalIntType, arg2: MalIntType) throws -> Bool {
    return arg1 > arg2
}

private func fn_gte(arg1: MalIntType, arg2: MalIntType) throws -> Bool {
    return arg1 >= arg2
}

private func fn_add(arg1: MalIntType, arg2: MalIntType) throws -> MalIntType {
    return arg1 + arg2
}

private func fn_subtract(arg1: MalIntType, arg2: MalIntType) throws -> MalIntType {
    return arg1 - arg2
}

private func fn_multiply(arg1: MalIntType, arg2: MalIntType) throws -> MalIntType {
    return arg1 * arg2
}

private func fn_divide(arg1: MalIntType, arg2: MalIntType) throws -> MalIntType {
    return arg1 / arg2
}

private func fn_timems() throws -> MalIntType {
    var time = timeval(tv_sec: 0, tv_usec: 0)
    let res = gettimeofday(&time, nil)
    if res == 0 {
        return (MalIntType(time.tv_sec) * 1_000_000 + MalIntType(time.tv_usec)) / 1000
    }
    return -1
}

private func fn_list(args: MalVarArgs) throws -> MalVal {
    return make_list(args.value)
}

private func fn_listQ(obj: MalVal) throws -> Bool {
    return is_list(obj)
}

private func fn_vector(args: MalVarArgs) throws -> MalVal {
    return make_vector(args.value)
}

private func fn_vectorQ(obj: MalVal) throws -> Bool {
    return is_vector(obj)
}

private func fn_hashmap(args: MalVarArgs) throws -> MalVal {
    return make_hashmap(args.value)
}

private func fn_hashmapQ(obj: MalVal) throws -> Bool {
    return is_hashmap(obj)
}

private func fn_assoc(hash: MalHashMap, args: MalVarArgs) throws -> MalVal {
   guard args.value.count % 2 == 0 else {
       try throw_error("expected even number of elements, got \(args.value.count)")
   }
   var new_dictionary = hash.hash
   for var index: MalIntType = 0; index < args.value.count; index += 2 {
       new_dictionary[try! args.value.nth(index)] = try! args.value.nth(index + 1)
   }
   return make_hashmap(new_dictionary)
}

private func fn_dissoc(hash: MalHashMap, args: MalVarArgs) throws -> MalVal {
    var new_dictionary = hash.hash
    for value in args.value {
        new_dictionary.removeValueForKey(value)
    }
    return make_hashmap(new_dictionary)
}

private func fn_get(obj: MalVal, key: MalVal) throws -> MalVal {
    if let as_vec = as_vectorQ(obj) {
        guard let index = as_integerQ(key) else {
            try throw_error("expected integer key for get(vector), got \(key)")
        }
        let n = as_inttype(index)
        guard n >= as_vec.count else { try throw_error("index out of range: \(n) >= \(as_vec.count)") }
        return try! as_vec.nth(n)
    }
    if let as_hash = as_hashmapQ(obj) {
        if let value = as_hash.value_for(key) { return value }
        return make_nil()
    }
    if is_nil(obj) {
        return obj
    }
    try throw_error("get called on unsupported type: \(obj)")
}

private func fn_containsQ(obj: MalVal, key: MalVal) throws -> MalVal {
    if let as_vec = as_vectorQ(obj) {
        guard let index = as_integerQ(key) else {
            try throw_error("expected integer key for contains(vector), got \(key)")
        }
        let n = as_inttype(index)
        return n < as_vec.count ? make_true() : make_false()
    }
    if let as_hash = as_hashmapQ(obj) {
        return as_hash.value_for(key) != nil ? make_true() : make_false()
    }
    try throw_error("contains? called on unsupported type: \(obj)")
}

private func fn_keys(hash: MalHashMap) throws -> MalVal {
    return hash.keys
}

private func fn_values(hash: MalHashMap) throws -> MalVal {
    return hash.values
}

private func fn_sequentialQ(obj: MalVal) throws -> Bool {
    return is_sequence(obj)
}

private func fn_cons(first: MalVal, rest: MalSequence) throws -> MalVal {
    return rest.cons(first)
}

private func fn_concat(args: MalVarArgs) throws -> MalVal {
    var result = make_list()
    for arg in args.value {
        guard let arg_as_seq = as_sequenceQ(arg) else {
            try throw_error("expected list, got \(arg)")
        }
        result = try! as_sequence(result).concat(arg_as_seq)
    }
    return result
}

private func fn_nth(list: MalSequence, index: MalIntType) throws -> MalVal {
    return try list.nth(index)
}

private func fn_first(arg: MalVal) throws -> MalVal {
    if is_nil(arg) {
        return arg
    }
    if let list = as_sequenceQ(arg) {
        return list.first()
    }
    try throw_error("expected list, got \(arg)")
}

private func fn_rest(list: MalSequence) throws -> MalVal {
    return list.rest()
}

private func fn_emptyQ(obj: MalVal) throws -> Bool {
    if let list = as_sequenceQ(obj) {
        return list.isEmpty
    }
    return true
}

private func fn_count(obj: MalVal) throws -> MalIntType {
    if is_nil(obj) {
        return 0
    }
    if let as_seq = as_sequenceQ(obj) {
        return as_seq.count
    }
    if let as_hash = as_hashmapQ(obj) {
        return as_hash.count
    }
    if let as_str = as_stringQ(obj) {
        return MalIntType(as_stringtype(as_str).characters.count)
    }
    return 0
}

private func fn_apply(args: MalVarArgs) throws -> MalVal {
    guard args.value.count >= 2 else {
        try throw_error("expected at least 2 arguments to apply, got \(args.value.count)")
    }

    let first   = args.value.first()
    let middle  = args.value.range_from(1, to: args.value.count - 1)
    let last    = args.value.last()

    guard let fn = as_functionQ(first) else {
        try throw_error("expected function for first argument to apply, got \(first)")
    }
    guard let seq = as_sequenceQ(last) else {
        try throw_error("expected sequence for last argument to apply, got \(last)")
    }
    let exprs = try! as_sequence(middle).concat(seq)
    return try fn.apply(as_sequence(exprs))
}

private func fn_map(fn: MalFunction, list: MalSequence) throws -> MalVal {
    var result = [MalVal]()
    result.reserveCapacity(Int(list.count))
    for var index: MalIntType = 0; index < list.count; ++index {
        let apply_res = try fn.apply(as_sequence(make_list_from(try! list.nth(index))))
        result.append(apply_res)
    }
    return make_list(result)
}

private func fn_conj(first: MalSequence, rest: MalVarArgs) throws -> MalVal {
    return try first.conj(rest.value)
}

private func fn_meta(obj: MalVal) throws -> MalVal {
    if let meta = get_meta(obj) {
        return meta
    }

    return make_nil()
}

private func fn_withmeta(form: MalVal, meta: MalVal) throws -> MalVal {
    return with_meta(form, meta)
}

private func fn_atom(obj: MalVal) throws -> MalVal {
    return make_atom(obj)
}

private func fn_atomQ(obj: MalVal) throws -> Bool {
    return is_atom(obj)
}

private func fn_deref(atom: MalAtom) throws -> MalVal {
    return atom.object
}

private func fn_resetBang(atom: MalAtom, obj: MalVal) throws -> MalVal {
    return atom.set_object(obj)
}

private func fn_swapBang(let atom: MalAtom, fn: MalFunction, rest: MalVarArgs) throws -> MalVal {
    var new_args = make_list_from(atom.object)
    new_args = try as_sequence(new_args).concat(rest.value)
    let result = try fn.apply(as_sequence(new_args))
    return atom.set_object(result)
}

//******************************************************************************
//
// The facility for invoking built-in functions makes use of a name ->
// function-pointer table (defined down below). The function-pointers accept a
// sequence of MalVals and return a MalVal as a result. Each built-in function
// that does actual work, on the other hand, may expect a different set of
// parameters of different types, and may naturally return a result of any type.
// In order to convert between these two types of interfaces, we have these
// unwrap_args functions. These functions implement the (MalSequence) -> MalVal
// interface expected by EVAL, and convert that information into Ints, Strings,
// etc. expected by the built-in functions.
//
//******************************************************************************

private func with_one_parameter(args: MalSequence, @noescape fn: (MalVal) throws -> MalVal) throws -> MalVal {
    guard args.count >= 1 else { try throw_error("expected at least 1 parameter, got \(args.count)") }
    let arg1 = try! args.nth(0)
    return try fn(arg1)
}

private func with_two_parameters(args: MalSequence, @noescape fn: (MalVal, MalVal) throws -> MalVal) throws -> MalVal {
    guard args.count >= 2 else { try throw_error("expected at least 2 parameter, got \(args.count)") }
    let arg1 = try! args.nth(0)
    let arg2 = try! args.nth(1)
    return try fn(arg1, arg2)
}

// ========== 0-parameter functions ==========

// () -> MalIntType

private func unwrap_args(args: MalSequence, @noescape forFunction fn: () throws -> MalIntType) throws -> MalVal {
    return make_integer(try fn())
}

// () -> MalVal

private func unwrap_args(args: MalSequence, @noescape forFunction fn: () throws -> MalVal) throws -> MalVal {
    return try fn()
}

// ========== 1-parameter functions ==========

// (MalAtom) -> MalVal

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalAtom) throws -> MalVal) throws -> MalVal {
    return try with_one_parameter(args) { (arg1) -> MalVal in
        guard let atom = as_atomQ(arg1) else {
            try throw_error("expected atom, got \(arg1)")
        }
        return try fn(atom)
    }
}

// (MalHashMap) -> MalVal

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalHashMap) throws -> MalVal) throws -> MalVal {
    return try with_one_parameter(args) { (arg1) -> MalVal in
        guard let hash = as_hashmapQ(arg1) else {
            try throw_error("expected hashmap, got \(arg1)")
        }
        return try fn(hash)
    }
}

// (MalSequence) -> MalVal

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalSequence) throws -> MalVal) throws -> MalVal {
    return try with_one_parameter(args) { (arg1) -> MalVal in
        guard let seq = as_sequenceQ(arg1) else {
            try throw_error("expected list, got \(arg1)")
        }
        return try fn(seq)
    }
}

// (MalVal) -> Bool

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalVal) throws -> Bool) throws -> MalVal {
    return try with_one_parameter(args) { (arg1) -> MalVal in
        return try fn(arg1) ? make_true() : make_false()
    }
}

// (MalVal) -> MalIntType

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalVal) throws -> MalIntType) throws -> MalVal {
    return try with_one_parameter(args) { (arg1) -> MalVal in
        return make_integer(try fn(arg1))
    }
}

// (MalVal) -> MalVal

func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalVal) throws -> MalVal) throws -> MalVal {
    return try with_one_parameter(args) { (arg1) -> MalVal in
        return try fn(arg1)
    }
}

// (String) -> MalVal

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (String) throws -> MalVal) throws -> MalVal {
    return try with_one_parameter(args) { (arg1) -> MalVal in
        guard let str = as_stringQ(arg1) else {
            try throw_error("expected string, got \(arg1)")
        }
        return try fn(as_stringtype(str))
    }
}

// (String) -> MalVal?

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (String) throws -> MalVal?) throws -> MalVal {
    return try with_one_parameter(args) { (arg1) -> MalVal in
        guard let str = as_stringQ(arg1) else {
            try throw_error("expected string, got \(arg1)")
        }
        let res = try fn(as_stringtype(str))
        return res != nil ? res! : make_nil()
    }
}

// (String) -> String

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (String) throws -> String) throws -> MalVal {
    return try with_one_parameter(args) { (arg1) -> MalVal in
        guard let str = as_stringQ(arg1) else {
            try throw_error("expected string, got \(arg1)")
        }
        return make_string(try fn(as_stringtype(str)))
    }
}

// (String) -> String?

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (String) throws -> String?) throws -> MalVal {
    return try with_one_parameter(args) { (arg1) -> MalVal in
        guard let str = as_stringQ(arg1) else {
            try throw_error("expected string, got \(arg1)")
        }
        let res = try fn(as_stringtype(str))
        return res != nil ? make_string(res!) : make_nil()
    }
}

// ========== 2-parameter functions ==========

// (MalIntType, MalIntType) -> Bool

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalIntType, MalIntType) throws -> Bool) throws -> MalVal {
    return try with_two_parameters(args) { (arg1, arg2) -> MalVal in
        guard let int1 = as_integerQ(arg1) else {
            try throw_error("expected number, got \(arg1)")
        }
        guard let int2 = as_integerQ(arg2) else {
            try throw_error("expected number, got \(arg2)")
        }
        return try fn(as_inttype(int1), as_inttype(int2)) ? make_true() : make_false()
    }
}

// (MalIntType, MalIntType) -> MalIntType

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalIntType, MalIntType) throws -> MalIntType) throws -> MalVal {
    return try with_two_parameters(args) { (arg1, arg2) -> MalVal in
        guard let int1 = as_integerQ(arg1) else {
            try throw_error("expected number, got \(arg1)")
        }
        guard let int2 = as_integerQ(arg2) else {
            try throw_error("expected number, got \(arg2)")
        }
        return make_integer(try fn(as_inttype(int1), as_inttype(int2)))
    }
}

// (MalAtom, MalVal) -> MalVal

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalAtom, MalVal) throws -> MalVal) throws -> MalVal {
    return try with_two_parameters(args) { (arg1, arg2) -> MalVal in
        guard let atom = as_atomQ(arg1) else {
            try throw_error("expected atom, got \(arg1)")
        }
        return try fn(atom, arg2)
    }
}

// (MalFunction, MalSequence) -> MalVal

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalFunction, MalSequence) throws -> MalVal) throws -> MalVal {
    return try with_two_parameters(args) { (arg1, arg2) -> MalVal in
        guard let fn1 = as_functionQ(arg1) else {
            try throw_error("expected function, got \(arg1)")
        }
        guard let seq2 = as_sequenceQ(arg2) else {
            try throw_error("expected sequence, got \(arg2)")
        }
        return try fn(fn1, seq2)
    }
}

// (MalSequence, MalIntType) -> MalVal

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalSequence, MalIntType) throws -> MalVal) throws -> MalVal {
    return try with_two_parameters(args) { (arg1, arg2) -> MalVal in
        guard let seq = as_sequenceQ(arg1) else {
            try throw_error("expected sequence, got \(arg1)")
        }
        guard let int = as_integerQ(arg2) else {
            try throw_error("expected number, got \(arg2)")
        }
        return try fn(seq, as_inttype(int))
    }
}

// (MalVal, MalSequence) -> MalVal

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalVal, MalSequence) throws -> MalVal) throws -> MalVal {
    return try with_two_parameters(args) { (arg1, arg2) -> MalVal in
        guard let seq = as_sequenceQ(arg2) else {
            try throw_error("expected sequence, got \(arg2)")
        }
        return try fn(arg1, seq)
    }
}

// (MalVal, MalVal) -> Bool

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalVal, MalVal) throws -> Bool) throws -> MalVal {
    return try with_two_parameters(args) { (arg1, arg2) -> MalVal in
        return try fn(arg1, arg2) ? make_true() : make_false()
    }
}

// (MalVal, MalVal) -> MalVal

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalVal, MalVal) throws -> MalVal) throws -> MalVal {
    return try with_two_parameters(args) { (arg1, arg2) -> MalVal in
        return try fn(arg1, arg2)
    }
}

// ========== Variadic functions ==========

// (MalVarArgs) -> ()

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalVarArgs) throws -> ()) throws -> MalVal {
    try fn(MalVarArgs(args))
    return make_nil()
}

// (MalVarArgs) -> String

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalVarArgs) throws -> String) throws -> MalVal {
    return make_string(try fn(MalVarArgs(args)))
}

// (MalVarArgs) -> MalVal

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalVarArgs) throws -> MalVal) throws -> MalVal {
    return try fn(MalVarArgs(args))
}

// (MalAtom, MalFunction, MalVarArgs) -> MalVal

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalAtom, MalFunction, MalVarArgs) throws -> MalVal) throws -> MalVal {
    return try with_two_parameters(args) { (arg1, arg2) -> MalVal in
        guard let atom = as_atomQ(arg1) else {
            try throw_error("expected atom, got \(arg1)")
        }
        guard let fn2 = as_functionQ(arg2) else {
            try throw_error("expected function, got \(arg2)")
        }
        return try fn(atom, fn2, MalVarArgs(as_sequence(args.rest()).rest()))
    }
}

// (MalHashMap, MalVarArgs) -> MalVal

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalHashMap, MalVarArgs) throws -> MalVal) throws -> MalVal {
    return try with_one_parameter(args) { (arg1) -> MalVal in
        guard let hash = as_hashmapQ(arg1) else {
            try throw_error("expected hashmap, got \(arg1)")
        }
        return try fn(hash, MalVarArgs(args.rest()))
    }
}

// (MalSequence, MalVarArgs) -> MalVal

private func unwrap_args(args: MalSequence, @noescape forFunction fn: (MalSequence, MalVarArgs) throws -> MalVal) throws -> MalVal {
    return try with_one_parameter(args) { (arg1) -> MalVal in
        guard let seq = as_sequenceQ(arg1) else {
            try throw_error("expected sequence, got \(arg1)")
        }
        return try fn(seq, MalVarArgs(args.rest()))
    }
}

// *o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*

let ns: [String: MalBuiltin.Signature] = [
    "=":            { try unwrap_args($0, forFunction: fn_eq) },
    "throw":        { try unwrap_args($0, forFunction: fn_throw) },

    "nil?":         { try unwrap_args($0, forFunction: fn_nilQ) },
    "true?":        { try unwrap_args($0, forFunction: fn_trueQ) },
    "false?":       { try unwrap_args($0, forFunction: fn_falseQ) },
    "symbol":       { try unwrap_args($0, forFunction: fn_symbol) },
    "symbol?":      { try unwrap_args($0, forFunction: fn_symbolQ) },
    "keyword":      { try unwrap_args($0, forFunction: fn_keyword) },
    "keyword?":     { try unwrap_args($0, forFunction: fn_keywordQ) },

    "pr-str":       { try unwrap_args($0, forFunction: fn_prstr) },
    "str":          { try unwrap_args($0, forFunction: fn_str) },
    "prn":          { try unwrap_args($0, forFunction: fn_prn) },
    "println":      { try unwrap_args($0, forFunction: fn_println) },
    "read-string":  { try unwrap_args($0, forFunction: fn_readstring) },
    "readline":     { try unwrap_args($0, forFunction: fn_readline) },
    "slurp":        { try unwrap_args($0, forFunction: fn_slurp) },

    "<":            { try unwrap_args($0, forFunction: fn_lt) },
    "<=":           { try unwrap_args($0, forFunction: fn_lte) },
    ">":            { try unwrap_args($0, forFunction: fn_gt) },
    ">=":           { try unwrap_args($0, forFunction: fn_gte) },
    "+":            { try unwrap_args($0, forFunction: fn_add) },
    "-":            { try unwrap_args($0, forFunction: fn_subtract) },
    "*":            { try unwrap_args($0, forFunction: fn_multiply) },
    "/":            { try unwrap_args($0, forFunction: fn_divide) },
    "time-ms":      { try unwrap_args($0, forFunction: fn_timems) },

    "list":         { try unwrap_args($0, forFunction: fn_list) },
    "list?":        { try unwrap_args($0, forFunction: fn_listQ) },
    "vector":       { try unwrap_args($0, forFunction: fn_vector) },
    "vector?":      { try unwrap_args($0, forFunction: fn_vectorQ) },
    "hash-map":     { try unwrap_args($0, forFunction: fn_hashmap) },
    "map?":         { try unwrap_args($0, forFunction: fn_hashmapQ) },
    "assoc":        { try unwrap_args($0, forFunction: fn_assoc) },
    "dissoc":       { try unwrap_args($0, forFunction: fn_dissoc) },
    "get":          { try unwrap_args($0, forFunction: fn_get) },
    "contains?":    { try unwrap_args($0, forFunction: fn_containsQ) },
    "keys":         { try unwrap_args($0, forFunction: fn_keys) },
    "vals":         { try unwrap_args($0, forFunction: fn_values) },

    "sequential?":  { try unwrap_args($0, forFunction: fn_sequentialQ) },
    "cons":         { try unwrap_args($0, forFunction: fn_cons) },
    "concat":       { try unwrap_args($0, forFunction: fn_concat) },
    "nth":          { try unwrap_args($0, forFunction: fn_nth) },
    "first":        { try unwrap_args($0, forFunction: fn_first) },
    "rest":         { try unwrap_args($0, forFunction: fn_rest) },
    "empty?":       { try unwrap_args($0, forFunction: fn_emptyQ) },
    "count":        { try unwrap_args($0, forFunction: fn_count) },
    "apply":        { try unwrap_args($0, forFunction: fn_apply) },
    "map":          { try unwrap_args($0, forFunction: fn_map) },
    "conj":         { try unwrap_args($0, forFunction: fn_conj) },

    "meta":         { try unwrap_args($0, forFunction: fn_meta) },
    "with-meta":    { try unwrap_args($0, forFunction: fn_withmeta) },
    "atom":         { try unwrap_args($0, forFunction: fn_atom) },
    "atom?":        { try unwrap_args($0, forFunction: fn_atomQ) },
    "deref":        { try unwrap_args($0, forFunction: fn_deref) },
    "reset!":       { try unwrap_args($0, forFunction: fn_resetBang) },
    "swap!":        { try unwrap_args($0, forFunction: fn_swapBang) },
]

func load_builtins(env: Environment) {
    for (name, fn) in ns {
        env.set(as_symbol(make_symbol(name)), make_builtin(fn))
    }
}
