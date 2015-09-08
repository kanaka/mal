//******************************************************************************
// MAL - reader
//******************************************************************************

import Foundation

let kSymbolWithMeta = MalSymbol(symbol: "with-meta")
let kSymbolDeref    = MalSymbol(symbol: "deref")

let token_pattern =
    "[[:space:],]*" +                       // Skip whitespace: a sequence of zero or more commas or [:space:]'s
    "(" +
        "~@" +                              // Literal "~@"
        "|" +
        "[\\[\\]{}()`'~^@]" +               // Punctuation: Any one of []{}()`'~^@
        "|" +
        "\"(?:\\\\.|[^\\\\\"])*\"" +        // Quoted string: characters other than \ or ", or any escaped characters
        "|" +
        ";.*" +                             // Comment: semicolon followed by anything
        "|" +
        "[^[:space:]\\[\\]{}()`'\",;]*" +   // Symbol, keyword, number, nil, true, false: any sequence of chars but [:space:] or []{}()`'",;
    ")"

let atom_pattern =
    "(^;.*$)" +                 // Comment
    "|" +
    "(^-?[0-9]+$)" +            // Integer
    "|" +
    "(^-?[0-9][0-9.]*$)" +      // Float
    "|" +
    "(^nil$)" +                 // nil
    "|" +
    "(^true$)" +                // true
    "|" +
    "(^false$)" +               // false
    "|" +
    "(^\".*\"$)" +              // String
    "|" +
    "(:.*)" +                   // Keyword
    "|" +
    "(^[^\"]*$)"                // Symbol

var token_regex: NSRegularExpression = NSRegularExpression(pattern:token_pattern, options:.allZeros, error:nil)!
var atom_regex: NSRegularExpression = NSRegularExpression(pattern:atom_pattern, options:.allZeros, error:nil)!

class Reader {

    init(_ tokens: [String]) {
        self.tokens = tokens
        self.index = 0
    }

    func next() -> String? {
        let token = peek()
        increment()
        return token
    }

    func peek() -> String? {
        if index < tokens.count {
            return tokens[index]
        }
        return nil
    }

    private func increment() {
        ++index
    }

    private let tokens: [String]
    private var index: Int
}

func tokenizer(s: String) -> [String] {
    var tokens = [String]()
    let range = NSMakeRange(0, count(s.utf16))
    let matches = token_regex.matchesInString(s, options:.allZeros, range:range)
    for match in matches as! [NSTextCheckingResult] {
        if match.range.length > 0 {
            let token = (s as NSString).substringWithRange(match.rangeAtIndex(1))
            tokens.append(token)
        }
    }
    return tokens
}

private func have_match_at(match: NSTextCheckingResult, index:Int) -> Bool {
    return Int64(match.rangeAtIndex(index).location) < LLONG_MAX
}

func read_atom(token: String) -> MalVal {
    let range = NSMakeRange(0, count(token.utf16))
    let matches = atom_regex.matchesInString(token, options:.allZeros, range:range)
    for match in matches as! [NSTextCheckingResult] {
        if have_match_at(match, 1) {                // Comment
            return MalComment(comment: token)
        } else if have_match_at(match, 2) {         // Integer
            if let value = NSNumberFormatter().numberFromString(token)?.longLongValue {
                return MalInteger(value: value)
            }
            return MalError(message: "invalid integer: \(token)")
        } else if have_match_at(match, 3) {         // Float
            if let value = NSNumberFormatter().numberFromString(token)?.doubleValue {
                return MalFloat(value: value)
            }
            return MalError(message: "invalid float: \(token)")
        } else if have_match_at(match, 4) {         // nil
            return MalNil()
        } else if have_match_at(match, 5) {         // true
            return MalTrue()
        } else if have_match_at(match, 6) {         // false
            return MalFalse()
        } else if have_match_at(match, 7) {         // String
            return MalString(escaped: token)
        } else if have_match_at(match, 8) {         // Keyword
            return MalKeyword(keyword: dropFirst(token))
        } else if have_match_at(match, 9) {         // Symbol
            return MalSymbol(symbol: token)
        }
    }
    return MalError(message: "Unknown token=\(token)")
}

func read_elements(r: Reader, open: String, close: String) -> ([MalVal]?, MalVal?) {
    var list = [MalVal]()
    while let token = r.peek() {
        if token == close {
            r.increment() // Consume the closing paren
            return (list, nil)
        } else {
            let item = read_form(r)
            if is_error(item) {
                return (nil, item)
            }
            if item.type != .TypeComment {
                list.append(item)
            }
        }
    }
    return (nil, MalError(message: "ran out of tokens -- possibly unbalanced ()'s"))
}

func read_list(r: Reader) -> MalVal {
    let (list, err) = read_elements(r, "(", ")")
    return err != nil ? err! : MalList(array: list!)
}

func read_vector(r: Reader) -> MalVal {
    let (list, err) = read_elements(r, "[", "]")
    return err != nil ? err! : MalVector(array: list!)
}

func read_hashmap(r: Reader) -> MalVal {
    let (list, err) = read_elements(r, "{", "}")
    return err != nil ? err! : MalHashMap(array: list!)
}

func common_quote(r: Reader, symbol: String) -> MalVal {
    let next = read_form(r)
    if is_error(next) { return next }
    return MalList(objects: MalSymbol(symbol: symbol), next)
}

func read_form(r: Reader) -> MalVal {
    if let token = r.next() {
        switch token {
            case "(":
                return read_list(r)
            case ")":
                return MalError(message: "unexpected \")\"")
            case "[":
                return read_vector(r)
            case "]":
                return MalError(message: "unexpected \"]\"")
            case "{":
                return read_hashmap(r)
            case "}":
                return MalError(message: "unexpected \"}\"")
            case "`":
                return common_quote(r, "quasiquote")
            case "'":
                return common_quote(r, "quote")
            case "~":
                return common_quote(r, "unquote")
            case "~@":
                return common_quote(r, "splice-unquote")
            case "^":
                let meta = read_form(r)
                if is_error(meta) { return meta }
                let form = read_form(r)
                if is_error(form) { return form }
                return MalList(objects: kSymbolWithMeta, form, meta)
            case "@":
                let form = read_form(r)
                if is_error(form) { return form }
                return MalList(objects: kSymbolDeref, form)
            default:
                return read_atom(token)
        }
    }
    return MalError(message: "ran out of tokens -- possibly unbalanced ()'s")
}

func read_str(s: String) -> MalVal {
    let tokens = tokenizer(s)
    let reader = Reader(tokens)
    let obj = read_form(reader)
    return obj
}
