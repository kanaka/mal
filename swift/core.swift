//******************************************************************************
// MAL - core
//******************************************************************************

import Foundation

typealias MalVarArgs = ArraySlice<MalVal>

func fn_eq(obj1: MalVal, obj2: MalVal) -> Bool {
    return obj1 == obj2
}

func fn_throw(exception: MalVal) -> MalVal {
    return MalError(object: exception)
}

func fn_nilQ(obj: MalVal) -> Bool {
    return is_nil(obj)
}

func fn_trueQ(obj: MalVal) -> Bool {
    return is_true(obj)
}

func fn_falseQ(obj: MalVal) -> Bool {
    return is_false(obj)
}

func fn_symbol(s: String) -> MalVal {
    return MalSymbol(symbol: s)
}

func fn_symbolQ(obj: MalVal) -> Bool {
    return is_symbol(obj)
}

func fn_keyword(s: MalVal) -> MalVal {
    if is_keyword(s) {
        return s
    }
    if is_string(s) {
        return MalKeyword(keyword: (s as! MalString).value)
    }
    return MalError(message: "expected string or keyword")
}

func fn_keywordQ(obj: MalVal) -> Bool {
    return is_keyword(obj)
}

func fn_prstr(args: MalVarArgs) -> String {
    let args_str_array = args.map { pr_str($0, true) }
    return " ".join(args_str_array)
}

func fn_str(args: MalVarArgs) -> String {
    let args_str_array = args.map { pr_str($0, false) }
    return "".join(args_str_array)
}

func fn_prn(args: MalVarArgs) {
    let args_str_array = args.map { pr_str($0, true) }
    let args_str = " ".join(args_str_array)
    println(args_str)
}

func fn_println(args: MalVarArgs) {
    let args_str_array = args.map { pr_str($0, false) }
    let args_str = " ".join(args_str_array)
    println(args_str)
}

func fn_readstring(s: String) -> MalVal {
    return read_str(s)
}

func fn_readline(s: String) -> String? {
    return _readline(s)
}

func fn_slurp(s: String) -> MalVal {
    var err: NSError? = nil
    let result = String(contentsOfFile:s, encoding: NSUTF8StringEncoding, error:&err)
    if result == nil {
        if err != nil {
            return MalError(message: "error reading file \(s): \(err!.localizedDescription)")
        }
        return MalError(message: "unknown error reading file \(s)")
    }
    return MalString(unescaped: result!);
}

func fn_lt(arg1: Int64, arg2: Int64) -> Bool {
    return arg1 < arg2
}

func fn_lte(arg1: Int64, arg2: Int64) -> Bool {
    return arg1 <= arg2
}

func fn_gt(arg1: Int64, arg2: Int64) -> Bool {
    return arg1 > arg2
}

func fn_gte(arg1: Int64, arg2: Int64) -> Bool {
    return arg1 >= arg2
}

func fn_add(arg1: Int64, arg2: Int64) -> Int64 {
    return arg1 + arg2
}

func fn_subtract(arg1: Int64, arg2: Int64) -> Int64 {
    return arg1 - arg2
}

func fn_multiply(arg1: Int64, arg2: Int64) -> Int64 {
    return arg1 * arg2
}

func fn_divide(arg1: Int64, arg2: Int64) -> Int64 {
    return arg1 / arg2
}

func fn_timems() -> Int64 {
    var time = timeval(tv_sec:0, tv_usec:0)
    let res = gettimeofday(&time, nil)
    if res == 0 {
        return (Int64(time.tv_sec) * 1_000_000 + Int64(time.tv_usec)) / 1000
    }
    return -1
}

func fn_list(args: MalVarArgs) -> MalVal {
    return MalList(slice: args)
}

func fn_listQ(obj: MalVal) -> Bool {
    return is_list(obj)
}

func fn_vector(args: MalVarArgs) -> MalVal {
    return MalVector(slice: args)
}

func fn_vectorQ(obj: MalVal) -> Bool {
    return is_vector(obj)
}

func fn_hashmap(args: MalVarArgs) -> MalVal {
    return MalHashMap(slice: args)
}

func fn_hashmapQ(obj: MalVal) -> Bool {
    return is_hashmap(obj)
}

func fn_assoc(hash: MalHashMap, args: MalVarArgs) -> MalVal {
   if args.count % 2 != 0 {
       return MalError(message: "expected even number of elements, got \(args.count)")
   }
   var new_dictionary = hash.hash
   for var index = 0; index < args.count; index += 2 {
       new_dictionary[args[index]] = args[index + 1]
   }
   return MalHashMap(hash: new_dictionary)
}

func fn_dissoc(hash: MalHashMap, args: MalVarArgs) -> MalVal {
   var new_dictionary = hash.hash
   for value in args {
       new_dictionary.removeValueForKey(value)
   }
   return MalHashMap(hash: new_dictionary)
}

func fn_get(obj: MalVal, key: MalVal) -> MalVal {
    if is_vector(obj) {
        if !is_integer(key) { return MalError(message: "expected integer key for get(vector), got \(key)") }
        let as_vector = obj as! MalVector
        let index = key as! MalInteger
        if Int(index.value) >= as_vector.count { return MalError(message: "index out of range: \(index) >= \(as_vector.count)") }
        return as_vector[Int(index.value)]
    }
    if is_hashmap(obj) {
        let as_hash = obj as! MalHashMap
        if let value = as_hash[key] { return value }
        return MalNil()
    }
    if is_nil(obj) {
        return obj
    }
    return MalError(message: "get called on unsupported type: \(obj)")
}

func fn_containsQ(obj: MalVal, key: MalVal) -> MalVal {
    if is_vector(obj) {
        if !is_integer(key) { return MalError(message: "expected integer key for contains(vector), got \(key)") }
        let as_vector = obj as! MalVector
        let index = key as! MalInteger
        return Int(index.value) < as_vector.count ? MalTrue() : MalFalse()
    }
    if is_hashmap(obj) {
        let as_hash = obj as! MalHashMap
        return as_hash[key] != nil ? MalTrue() : MalFalse()
    }
    return MalError(message: "contains? called on unsupported type: \(obj)")
}

func fn_keys(hash: MalHashMap) -> MalVal {
    return MalList(array: hash.hash.keys.array)
}

func fn_values(hash: MalHashMap) -> MalVal {
    return MalList(array: hash.hash.values.array)
}

func fn_sequentialQ(obj: MalVal) -> Bool {
    return is_sequence(obj)
}

func fn_cons(first:MalVal, rest:MalSequence) -> MalVal {
    var new_elements = [MalVal]([first])
    new_elements.extend(rest.slice)
    return MalList(array: new_elements)
}

func fn_concat(args: MalVarArgs) -> MalVal {
    var result = [MalVal]()
    for arg in args {
        if !is_sequence(arg) { return MalError(message: "expected list, got \(arg)") }
        result.extend((arg as! MalSequence).slice)
    }
    return MalList(array: result)
}

func fn_nth(list: MalSequence, index: Int) -> MalVal {
    return list[index]
}

func fn_first(arg: MalVal) -> MalVal {
    if is_nil(arg) {
        return arg
    }
    if is_sequence(arg) {
        let list = arg as! MalSequence
        return list.first()
    }
    return MalError(message: "expected list, got \(arg)")
}

func fn_rest(list: MalSequence) -> MalVal {
    return MalList(slice: list.rest().slice)
}

func fn_emptyQ(obj: MalVal) -> Bool {
    if is_sequence(obj) {
        let list = obj as! MalSequence
        return list.isEmpty
    }
    return true
}

func fn_count(obj: MalVal) -> Int64 {
    if is_nil(obj) {
        return 0
    }
    if is_sequence(obj) {
        let as_seq = obj as! MalSequence
        return Int64(as_seq.count)
    }
    if is_hashmap(obj) {
        let hash = obj as! MalHashMap
        return Int64(hash.count)
    }
    if is_string(obj) {
        let string = obj as! MalString
        return Int64(count(string.value.utf16))
    }
    return 0
}

func fn_apply(args: MalVarArgs) -> MalVal {
    if args.count < 2 { return MalError(message: "expected at least 2 arguments to apply, got \(args.count)") }
    let first   = args[0]
    var middle  = args[1 ..< args.count - 1]
    let last    = args[args.count - 1]
    if !is_function(first) { return MalError(message: "expected function for first argument to apply, got \(first)") }
    if !is_sequence(last) { return MalError(message: "expected sequence for last argument to apply, got \(last)") }
    middle.extend((last as! MalSequence).slice)
    return (first as! MalFunction).apply(MalList(slice: middle))
}

func fn_map(fn: MalFunction, list: MalSequence) -> MalVal {
    var result = [MalVal]()
    result.reserveCapacity(list.count)
    for var index = 0; index < list.count; ++index {
        let apply_res = fn.apply(MalList(slice: list.slice[index...index]))
        if is_error(apply_res) { return apply_res }
        result.append(apply_res)
    }
    return MalList(array: result)
}

func fn_conj(first: MalSequence, rest: MalVarArgs) -> MalVal {
    var result = [MalVal]()
    result.reserveCapacity(first.count + rest.count)
    if is_list(first) {
        for var index = 0; index < rest.count; ++index {
            result.append(rest[rest.count - 1 - index])
        }
        result.extend(first.slice)
        return MalList(array: result)
    } else {
        result.extend(first.slice)
        result.extend(rest)
        return MalVector(array: result)
    }
}

func fn_meta(obj: MalVal) -> MalVal {
    return obj.meta != nil ? obj.meta! : MalNil()
}

func fn_withmeta(form:MalVal, meta:MalVal) -> MalVal {
    var new_form = form.clone()
    new_form.meta = meta
    return new_form
}

func fn_atom(obj: MalVal) -> MalVal {
    return MalAtom(object: obj)
}

func fn_atomQ(obj: MalVal) -> Bool {
    return is_atom(obj)
}

func fn_deref(form:MalVal) -> MalVal {
    if !is_atom(form) { return MalError(message: "expected atom, got \(form)") }
    return (form as! MalAtom).value
}

func fn_resetBang(atom: MalAtom, obj: MalVal) -> MalVal {
    atom.value = obj
    return obj
}

func fn_swapBang(var atom: MalAtom, fn: MalFunction, rest: MalVarArgs) -> MalVal {
    var new_args = [MalVal]([atom.value])
    new_args.extend(rest)
    let result = fn.apply(MalList(array: new_args))
    if is_error(result) { return result }
    atom.value = result
    return result
}

//******************************************************************************
//
// The facility for invoking built-in functions makes use of a name ->
// function-pointer table (defined down below). The function-pointers accept a
// sequence of MalVals and return a MalVal as a result. Each built-in function
// that does actual work, on the other hand, may expect a different set of
// parameters of different types, and may naturally return a result of any type.
// In order to convert between these two types of interfaces, we have these
// unwrap functions. These functions implement the (MalSequence) -> MalVal
// interface expected by EVAL, and convert that information into Ints, Strings,
// etc. expected by the built-in functions.
//
//******************************************************************************

func with_one_parameter(args: MalSequence, fn: (MalVal) -> MalVal) -> MalVal {
    if args.count < 1 { return MalError(message: "expected at least 1 parameter, got \(args.count)") }
    let arg1 = args[0]
    //let rest = args[1..<args.count]
    return fn(arg1)
}

func with_two_parameters(args: MalSequence, fn: (MalVal, MalVal) -> MalVal) -> MalVal {
    if args.count < 2 { return MalError(message: "expected at least 2 parameter, got \(args.count)") }
    let arg1 = args[0]
    let arg2 = args[1]
    //let rest = args[2..<args.count]
    return fn(arg1, arg2)
}

// ========== 0-parameter functions ==========

// () -> Int64

func unwrap(args: MalSequence, fn: () -> Int64) -> MalVal {
    return MalInteger(value: fn())
}

// () -> MalVal

func unwrap(args: MalSequence, fn: () -> MalVal) -> MalVal {
    return fn()
}

// ========== 1-parameter functions ==========

// (MalHashMap) -> MalVal

func unwrap(args: MalSequence, fn: (MalHashMap) -> MalVal) -> MalVal {
    return with_one_parameter(args) { (arg1) -> MalVal in
        if !is_hashmap(arg1) { return MalError(message: "expected hashmap, got \(arg1)") }
        return fn(arg1 as! MalHashMap)
    }
}

// (MalSequence) -> MalVal

func unwrap(args: MalSequence, fn: (MalSequence) -> MalVal) -> MalVal {
    return with_one_parameter(args) { (arg1) -> MalVal in
        if !is_sequence(arg1) { return MalError(message: "expected list, got \(arg1)") }
        return fn(arg1 as! MalSequence)
    }
}

// (MalVal) -> Bool

func unwrap(args: MalSequence, fn: (MalVal) -> Bool) -> MalVal {
    return with_one_parameter(args) { (arg1) -> MalVal in
        return fn(arg1) ? MalTrue() : MalFalse()
    }
}

// (MalVal) -> Int64

func unwrap(args: MalSequence, fn: (MalVal) -> Int64) -> MalVal {
    return with_one_parameter(args) { (arg1) -> MalVal in
        return MalInteger(value: fn(arg1))
    }
}

// (MalVal) -> MalVal

func unwrap(args: MalSequence, fn: (MalVal) -> MalVal) -> MalVal {
    return with_one_parameter(args) { (arg1) -> MalVal in
        return fn(arg1)
    }
}

// (String) -> MalVal

func unwrap(args: MalSequence, fn: (String) -> MalVal) -> MalVal {
    return with_one_parameter(args) { (arg1) -> MalVal in
        if !is_string(arg1) { return MalError(message: "expected string, got \(arg1)") }
        return fn((arg1 as! MalString).value)
    }
}

// (String) -> MalVal?

func unwrap(args: MalSequence, fn: (String) -> MalVal?) -> MalVal {
    return with_one_parameter(args) { (arg1) -> MalVal in
        if !is_string(arg1) { return MalError(message: "expected string, got \(arg1)") }
        let res = fn((arg1 as! MalString).value)
        return res != nil ? res! : MalNil()
    }
}

// (String) -> String

func unwrap(args: MalSequence, fn: (String) -> String) -> MalVal {
    return with_one_parameter(args) { (arg1) -> MalVal in
        if !is_string(arg1) { return MalError(message: "expected string, got \(arg1)") }
        return MalString(unescaped: fn((arg1 as! MalString).value))
    }
}

// (String) -> String?

func unwrap(args: MalSequence, fn: (String) -> String?) -> MalVal {
    return with_one_parameter(args) { (arg1) -> MalVal in
        if !is_string(arg1) { return MalError(message: "expected string, got \(arg1)") }
        let res = fn((arg1 as! MalString).value)
        return res != nil ? MalString(unescaped:res!) : MalNil()
    }
}

// ========== 2-parameter functions ==========

// (Int64, Int64) -> Bool

func unwrap(args: MalSequence, fn: (Int64, Int64) -> Bool) -> MalVal {
    return with_two_parameters(args) { (arg1, arg2) -> MalVal in
        if !is_integer(arg1) { return MalError(message: "expected number, got \(arg1)") }
        if !is_integer(arg2) { return MalError(message: "expected number, got \(arg2)") }
        return fn((arg1 as! MalInteger).value, (arg2 as! MalInteger).value) ? MalTrue() : MalFalse()
    }
}

// (Int64, Int64) -> Int64

func unwrap(args: MalSequence, fn: (Int64, Int64) -> Int64) -> MalVal {
    return with_two_parameters(args) { (arg1, arg2) -> MalVal in
        if !is_integer(arg1) { return MalError(message: "expected number, got \(arg1)") }
        if !is_integer(arg2) { return MalError(message: "expected number, got \(arg2)") }
        return MalInteger(value: fn((arg1 as! MalInteger).value, (arg2 as! MalInteger).value))
    }
}

// (MalAtom, MalVal) -> MalVal

func unwrap(args: MalSequence, fn: (MalAtom, MalVal) -> MalVal) -> MalVal {
    return with_two_parameters(args) { (arg1, arg2) -> MalVal in
        if !is_atom(arg1) { return MalError(message: "expected atom, got \(arg1)") }
        return fn((arg1 as! MalAtom), arg2)
    }
}

// (MalFunction, MalSequence) -> MalVal

func unwrap(args: MalSequence, fn: (MalFunction, MalSequence) -> MalVal) -> MalVal {
    return with_two_parameters(args) { (arg1, arg2) -> MalVal in
        if !is_function(arg1) { return MalError(message: "expected function, got \(arg1)") }
        if !is_sequence(arg2) { return MalError(message: "expected sequence, got \(arg2)") }
        return fn((arg1 as! MalFunction), (arg2 as! MalSequence))
    }
}

// (MalSequence, Int) -> MalVal

func unwrap(args: MalSequence, fn: (MalSequence, Int) -> MalVal) -> MalVal {
    return with_two_parameters(args) { (arg1, arg2) -> MalVal in
        if !is_sequence(arg1) { return MalError(message: "expected sequence, got \(arg1)") }
        if !is_integer(arg2)  { return MalError(message: "expected number, got \(arg2)") }
        return fn((arg1 as! MalSequence), Int((arg2 as! MalInteger).value))
    }
}

// (MalVal, MalSequence) -> MalVal

func unwrap(args: MalSequence, fn: (MalVal, MalSequence) -> MalVal) -> MalVal {
    return with_two_parameters(args) { (arg1, arg2) -> MalVal in
        if !is_sequence(arg2) { return MalError(message: "expected sequence, got \(arg2)") }
        return fn(arg1, (arg2 as! MalSequence))
    }
}

// (MalVal, MalVal) -> Bool

func unwrap(args: MalSequence, fn: (MalVal, MalVal) -> Bool) -> MalVal {
    return with_two_parameters(args) { (arg1, arg2) -> MalVal in
        return fn(arg1, arg2) ? MalTrue() : MalFalse()
    }
}

// (MalVal, MalVal) -> MalVal

func unwrap(args: MalSequence, fn: (MalVal, MalVal) -> MalVal) -> MalVal {
    return with_two_parameters(args) { (arg1, arg2) -> MalVal in
        return fn(arg1, arg2)
    }
}

// ========== Variadic functions ==========

// (MalVarArgs) -> ()

func unwrap(args: MalSequence, fn: (MalVarArgs) -> ()) -> MalVal {
    fn(args.slice)
    return MalNil()
}

// (MalVarArgs) -> String

func unwrap(args: MalSequence, fn: (MalVarArgs) -> String) -> MalVal {
    return MalString(unescaped: fn(args.slice))
}

// (MalVarArgs) -> MalVal

func unwrap(args: MalSequence, fn: (MalVarArgs) -> MalVal) -> MalVal {
    return fn(args.slice)
}

// (MalAtom, MalFunction, MalVarArgs) -> MalVal

func unwrap(args: MalSequence, fn: (MalAtom, MalFunction, MalVarArgs) -> MalVal) -> MalVal {
    return with_two_parameters(args) { (arg1, arg2) -> MalVal in
        if !is_atom(arg1) { return MalError(message: "expected atom, got \(arg1)") }
        if !is_function(arg2) { return MalError(message: "expected function, got \(arg2)") }
        return fn((arg1 as! MalAtom), (arg2 as! MalFunction), args[2..<args.count])
    }
}

// (MalHashMap, MalVarArgs) -> MalVal

func unwrap(args: MalSequence, fn: (MalHashMap, MalVarArgs) -> MalVal) -> MalVal {
    return with_one_parameter(args) { (arg1) -> MalVal in
        if !is_hashmap(arg1) { return MalError(message: "expected hashmap, got \(arg1)") }
        return fn((arg1 as! MalHashMap), args[1..<args.count])
    }
}

// (MalSequence, MalVarArgs) -> MalVal

func unwrap(args: MalSequence, fn: (MalSequence, MalVarArgs) -> MalVal) -> MalVal {
    return with_one_parameter(args) { (arg1) -> MalVal in
        if !is_sequence(arg1) { return MalError(message: "expected sequence, got \(arg1)") }
        return fn((arg1 as! MalSequence), args[1..<args.count])
    }
}

// *o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*o*

let ns: [String: MalBuiltin.BuiltinSignature] = [
    "=":            { unwrap($0, fn_eq) },
    "throw":        { unwrap($0, fn_throw) },

    "nil?":         { unwrap($0, fn_nilQ) },
    "true?":        { unwrap($0, fn_trueQ) },
    "false?":       { unwrap($0, fn_falseQ) },
    "symbol":       { unwrap($0, fn_symbol) },
    "symbol?":      { unwrap($0, fn_symbolQ) },
    "keyword":      { unwrap($0, fn_keyword) },
    "keyword?":     { unwrap($0, fn_keywordQ) },

    "pr-str":       { unwrap($0, fn_prstr) },
    "str":          { unwrap($0, fn_str) },
    "prn":          { unwrap($0, fn_prn) },
    "println":      { unwrap($0, fn_println) },
    "read-string":  { unwrap($0, fn_readstring) },
    "readline":     { unwrap($0, fn_readline) },
    "slurp":        { unwrap($0, fn_slurp) },

    "<":            { unwrap($0, fn_lt) },
    "<=":           { unwrap($0, fn_lte) },
    ">":            { unwrap($0, fn_gt) },
    ">=":           { unwrap($0, fn_gte) },
    "+":            { unwrap($0, fn_add) },
    "-":            { unwrap($0, fn_subtract) },
    "*":            { unwrap($0, fn_multiply) },
    "/":            { unwrap($0, fn_divide) },
    "time-ms":      { unwrap($0, fn_timems) },

    "list":         { unwrap($0, fn_list) },
    "list?":        { unwrap($0, fn_listQ) },
    "vector":       { unwrap($0, fn_vector) },
    "vector?":      { unwrap($0, fn_vectorQ) },
    "hash-map":     { unwrap($0, fn_hashmap) },
    "map?":         { unwrap($0, fn_hashmapQ) },
    "assoc":        { unwrap($0, fn_assoc) },
    "dissoc":       { unwrap($0, fn_dissoc) },
    "get":          { unwrap($0, fn_get) },
    "contains?":    { unwrap($0, fn_containsQ) },
    "keys":         { unwrap($0, fn_keys) },
    "vals":         { unwrap($0, fn_values) },

    "sequential?":  { unwrap($0, fn_sequentialQ) },
    "cons":         { unwrap($0, fn_cons) },
    "concat":       { unwrap($0, fn_concat) },
    "nth":          { unwrap($0, fn_nth) },
    "first":        { unwrap($0, fn_first) },
    "rest":         { unwrap($0, fn_rest) },
    "empty?":       { unwrap($0, fn_emptyQ) },
    "count":        { unwrap($0, fn_count) },
    "apply":        { unwrap($0, fn_apply) },
    "map":          { unwrap($0, fn_map) },
    "conj":         { unwrap($0, fn_conj) },

    "meta":         { unwrap($0, fn_meta) },
    "with-meta":    { unwrap($0, fn_withmeta) },
    "atom":         { unwrap($0, fn_atom) },
    "atom?":        { unwrap($0, fn_atomQ) },
    "deref":        { unwrap($0, fn_deref) },
    "reset!":       { unwrap($0, fn_resetBang) },
    "swap!":        { unwrap($0, fn_swapBang) },
]

func load_builtins(env: Environment) {
    for (name, fn) in ns {
        env.set(MalSymbol(symbol: name), MalBuiltin(function: fn))
    }
}
