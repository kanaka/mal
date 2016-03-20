let token_delim: Set<Character> = [
    ";", ",", "\"", "`", " ", "\n", "{", "}", "(", ")", "[", "]"
]

let int_char: Set<Character> = [
    "-", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
]

let float_char: Set<Character> = [
    ".", "-", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
]

let whitespace: Set<Character> = [" ", "\t", "\n", ","]

class Reader {
    var str: String
    var pos: String.Index
    init(_ str: String) {
        self.str = str
        pos = str.startIndex
    }
    func next() { pos = pos.successor() }
}

func read_int(rdr: Reader) -> MalVal {
    let start = rdr.pos
    for cidx in rdr.pos..<rdr.str.endIndex {
        rdr.pos = cidx
        if !int_char.contains(rdr.str[cidx]) { break }
        rdr.pos = cidx.successor()
    }
    let matchStr = rdr.str.substringWithRange(start..<rdr.pos)
    if matchStr == "-" {
        return MalVal.MalSymbol("-")
    } else {
        return MalVal.MalInt(Int(matchStr)!)
    }
}

func skip_whitespace_and_comments(rdr: Reader) {
    var in_comment = false
    for cidx in rdr.pos..<rdr.str.endIndex {
        rdr.pos = cidx
        if in_comment {
            if rdr.str[rdr.pos] == "\n" {
                in_comment = false
            }
        } else if rdr.str[rdr.pos] == ";" {
            in_comment = true
        } else {
            if !whitespace.contains(rdr.str[rdr.pos]) { break }
        }
    }
}

func read_string(rdr: Reader) throws -> MalVal {
    let start = rdr.pos
    var escaped = false
    if rdr.str[rdr.pos] != "\"" {
        throw MalError.Reader(msg: "read_string call on non-string")
    }
    for cidx in rdr.pos.successor()..<rdr.str.endIndex {
        rdr.pos = cidx.successor()
        if escaped {
            escaped = false
            continue
        }
        if rdr.str[cidx] == "\\" { escaped = true }
        if rdr.str[cidx] == "\"" { break }
    }
    if rdr.pos > rdr.str.endIndex {
        throw MalError.Reader(msg: "Expected '\"', got EOF")
    }
    let matchStr = rdr.str.substringWithRange(
        start.successor()..<rdr.pos.predecessor())
    let s1 = matchStr.stringByReplacingOccurrencesOfString(
        "\\\"", withString: "\"")
    let s2 = s1.stringByReplacingOccurrencesOfString(
        "\\n", withString: "\n")
    let s3 = s2.stringByReplacingOccurrencesOfString(
        "\\\\", withString: "\\")
    return MalVal.MalString(s3)
}

func read_token(rdr: Reader) -> String {
    let start = rdr.pos
    for cidx in rdr.pos..<rdr.str.endIndex {
        rdr.pos = cidx
        if token_delim.contains(rdr.str[cidx]) { break }
        rdr.pos = cidx.successor()
    }
    return rdr.str.substringWithRange(start..<rdr.pos)
}

func read_symbol(rdr: Reader) throws -> MalVal {
   let tok = read_token(rdr)
    switch tok {
        case "nil": return MalVal.MalNil
        case "true": return MalVal.MalTrue
        case "false": return MalVal.MalFalse
        default: return MalVal.MalSymbol(tok)
    }
}

func read_atom(rdr: Reader) throws -> MalVal {
    if rdr.str.characters.count == 0 {
        throw MalError.Reader(msg: "Empty string passed to read_atom")
    }
    switch rdr.str[rdr.pos] {
    case "-" where !int_char.contains(rdr.str[rdr.pos.successor()]):
        return try read_symbol(rdr)
    case let c where int_char.contains(c):
        return read_int(rdr)
    case "\"":
        return try read_string(rdr)
    case ":":
        rdr.next()
        return MalVal.MalString("\u{029e}\(read_token(rdr))")
    default:
        return try read_symbol(rdr)
    }
}

func read_list(rdr: Reader, start: Character = "(", end: Character = ")") throws -> Array<MalVal> {
    if rdr.str[rdr.pos] != start {
        throw MalError.Reader(msg: "expected '\(start)'")
    }
    rdr.next()
    var lst: [MalVal] = []
    while rdr.pos < rdr.str.endIndex {
        if (rdr.str[rdr.pos] == end) { break }
        lst.append(try read_form(rdr))
    }
    if rdr.pos >= rdr.str.endIndex {
        throw MalError.Reader(msg: "Expected '\(end)', got EOF")
    }
    rdr.next()
    return lst
}

func read_form(rdr: Reader) throws -> MalVal {
    if rdr.str.characters.count == 0 {
        throw MalError.Reader(msg: "Empty string passed to read_form")
    }
    //print("read_form: \(rdr.pos): \(rdr.str[rdr.pos])")
    skip_whitespace_and_comments(rdr)
    var res: MalVal
    switch rdr.str[rdr.pos] {
    // reader macros/transforms
    case "'":
        rdr.next()
        return list([MalVal.MalSymbol("quote"), try read_form(rdr)])
    case "`":
        rdr.next()
        return list([MalVal.MalSymbol("quasiquote"), try read_form(rdr)])
    case "~":
        switch rdr.str[rdr.pos.successor()] {
        case "@":
            rdr.next()
            rdr.next()
            return list([MalVal.MalSymbol("splice-unquote"),
                         try read_form(rdr)])
        default:
            rdr.next()
            return list([MalVal.MalSymbol("unquote"),
                         try read_form(rdr)])
        }
    case "^":
        rdr.next()
        let meta = try read_form(rdr)
        return list([MalVal.MalSymbol("with-meta"),
                     try read_form(rdr),
                     meta])
    case "@":
        rdr.next()
        return list([MalVal.MalSymbol("deref"),
                     try read_form(rdr)])

    // list
    case "(": res = list(try read_list(rdr))
    case ")": throw MalError.Reader(msg: "unexpected ')'")

    // vector
    case "[": res = vector(try read_list(rdr, start: "[", end: "]"))
    case "]": throw MalError.Reader(msg: "unexpected ']'")

    // hash-map
    case "{": res = try hash_map(try read_list(rdr, start: "{", end: "}"))
    case "}": throw MalError.Reader(msg: "unexpected '}'")

    // atom
    default: res = try read_atom(rdr)
    }
    skip_whitespace_and_comments(rdr)
    return res
}

func read_str(str: String) throws -> MalVal {
    return try read_form(Reader(str))
}
