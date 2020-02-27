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
    func next() { pos = str.index(after: pos) }
}

func read_int(_ rdr: Reader) -> MalVal {
    let start = rdr.pos
    var cidx = rdr.pos
    while cidx < rdr.str.endIndex {
        if !int_char.contains(rdr.str[cidx]) { break }
        cidx = rdr.str.index(after: cidx)
        rdr.pos = cidx
    }
    let matchStr = rdr.str.substring(with: start..<rdr.pos)
    if matchStr == "-" {
        return MalVal.MalSymbol("-")
    } else {
        return MalVal.MalInt(Int(matchStr)!)
    }
}

func skip_whitespace_and_comments(_ rdr: Reader) {
    var in_comment = false
    var cidx = rdr.pos
    while cidx < rdr.str.endIndex {
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
        cidx = rdr.str.index(after: cidx)
    }
}

func read_string(_ rdr: Reader) throws -> MalVal {
    let start = rdr.pos
    var escaped = false
    if rdr.str[rdr.pos] != "\"" {
        throw MalError.Reader(msg: "read_string call on non-string")
    }
    var cidx = rdr.str.index(after: rdr.pos)
    while cidx < rdr.str.endIndex {
        rdr.pos = rdr.str.index(after: cidx)
        if escaped {
            escaped = false
            cidx = rdr.pos
            continue
        }
        if rdr.str[cidx] == "\\" { escaped = true }
        if rdr.str[cidx] == "\"" { break }
        cidx = rdr.pos
    }
    if cidx >= rdr.str.endIndex || rdr.str[rdr.str.index(before: rdr.pos)] != "\"" {
        throw MalError.Reader(msg: "Expected '\"', got EOF")
    }
    let matchStr = rdr.str.substring(with: 
        rdr.str.index(after: start)..<rdr.str.index(before: rdr.pos))
    let s0 = matchStr.replacingOccurrences(of: "\\\\", with: "\u{029e}")
    let s1 = s0.replacingOccurrences(of: "\\\"", with: "\"")
    let s2 = s1.replacingOccurrences(of: "\\n", with: "\n")
    let s3 = s2.replacingOccurrences(of: "\u{029e}", with: "\\")
    return MalVal.MalString(s3)
}

func read_token(_ rdr: Reader) -> String {
    let start = rdr.pos
    var cidx = rdr.pos
    while cidx < rdr.str.endIndex {
        rdr.pos = cidx
        if token_delim.contains(rdr.str[cidx]) { break }
        cidx = rdr.str.index(after: cidx)
        rdr.pos = cidx
    }
    return rdr.str.substring(with: start..<rdr.pos)
}

func read_symbol(_ rdr: Reader) throws -> MalVal {
   let tok = read_token(rdr)
    switch tok {
        case "nil": return MalVal.MalNil
        case "true": return MalVal.MalTrue
        case "false": return MalVal.MalFalse
        default: return MalVal.MalSymbol(tok)
    }
}

func read_atom(_ rdr: Reader) throws -> MalVal {
    if rdr.str.characters.count == 0 {
        throw MalError.Reader(msg: "Empty string passed to read_atom")
    }
    switch rdr.str[rdr.pos] {
    case "-" where rdr.str.characters.count == 1 || !int_char.contains(rdr.str[rdr.str.index(after: rdr.pos)]):
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

func read_list(_ rdr: Reader, start: Character = "(", end: Character = ")") throws -> Array<MalVal> {
    if rdr.str[rdr.pos] != start {
        throw MalError.Reader(msg: "expected '\(start)'")
    }
    rdr.next()
    skip_whitespace_and_comments(rdr)
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

func read_form(_ rdr: Reader) throws -> MalVal {
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
        switch rdr.str[rdr.str.index(after: rdr.pos)] {
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

func read_str(_ str: String) throws -> MalVal {
    return try read_form(Reader(str))
}
