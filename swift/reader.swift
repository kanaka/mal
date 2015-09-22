//******************************************************************************
// MAL - reader
//******************************************************************************

import Foundation

private let kSymbolWithMeta = make_symbol("with-meta")
private let kSymbolDeref    = make_symbol("deref")

private let token_pattern =
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

private let atom_pattern =
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

private var token_regex: NSRegularExpression = try! NSRegularExpression(pattern: token_pattern, options: NSRegularExpressionOptions())
private var atom_regex: NSRegularExpression = try! NSRegularExpression(pattern: atom_pattern, options: NSRegularExpressionOptions())

private final class Reader {

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

private func tokenizer(s: String) -> [String] {
    var tokens = [String]()
    let range = NSMakeRange(0, s.characters.count)
    let matches = token_regex.matchesInString(s, options: NSMatchingOptions(), range: range)
    for match in matches {
        if match.range.length > 0 {
            let token = (s as NSString).substringWithRange(match.rangeAtIndex(1))
            tokens.append(token)
        }
    }
    return tokens
}

private func have_match(match: NSTextCheckingResult, at_index index: Int) -> Bool {
    return Int64(match.rangeAtIndex(index).location) < LLONG_MAX
}

private func read_atom(token: String) throws -> MalVal {
    let range = NSMakeRange(0, token.characters.count)
    let matches = atom_regex.matchesInString(token, options: NSMatchingOptions(), range: range)
    for match in matches {
        if have_match(match, at_index: 1) {                // Comment
            return make_comment()
        } else if have_match(match, at_index: 2) {         // Integer
            guard let value = NSNumberFormatter().numberFromString(token)?.longLongValue else {
                try throw_error("invalid integer: \(token)")
            }
            return make_integer(value)
        } else if have_match(match, at_index: 3) {         // Float
            guard let value = NSNumberFormatter().numberFromString(token)?.doubleValue else {
                try throw_error("invalid float: \(token)")
            }
            return make_float(value)
        } else if have_match(match, at_index: 4) {         // nil
            return make_nil()
        } else if have_match(match, at_index: 5) {         // true
            return make_true()
        } else if have_match(match, at_index: 6) {         // false
            return make_false()
        } else if have_match(match, at_index: 7) {         // String
            return make_string(unescape(token))
        } else if have_match(match, at_index: 8) {         // Keyword
            return make_keyword(token[token.startIndex.successor() ..< token.endIndex])
        } else if have_match(match, at_index: 9) {         // Symbol
            return make_symbol(token)
        }
    }
    try throw_error("Unknown token=\(token)")
}

private func read_elements(r: Reader, _ open: String, _ close: String) throws -> [MalVal] {
    var list = [MalVal]()
    while let token = r.peek() {
        if token == close {
            r.increment() // Consume the closing paren
            return list
        } else {
            let item = try read_form(r)
            if !is_comment(item) {
                list.append(item)
            }
        }
    }
    try throw_error("ran out of tokens -- possibly unbalanced ()'s")
}

private func read_list(r: Reader) throws -> MalVal {
    return make_list(try read_elements(r, "(", ")"))
}

private func read_vector(r: Reader) throws -> MalVal {
    return make_vector(try read_elements(r, "[", "]"))
}

private func read_hashmap(r: Reader) throws -> MalVal {
    return make_hashmap(try read_elements(r, "{", "}"))
}

private func common_quote(r: Reader, _ symbol: String) throws -> MalVal {
    let next = try read_form(r)
    return make_list_from(make_symbol(symbol), next)
}

private func read_form(r: Reader) throws -> MalVal {
    if let token = r.next() {
        switch token {
            case "(":
                return try read_list(r)
            case ")":
                try throw_error("unexpected \")\"")
            case "[":
                return try read_vector(r)
            case "]":
                try throw_error("unexpected \"]\"")
            case "{":
                return try read_hashmap(r)
            case "}":
                try throw_error("unexpected \"}\"")
            case "`":
                return try common_quote(r, "quasiquote")
            case "'":
                return try common_quote(r, "quote")
            case "~":
                return try common_quote(r, "unquote")
            case "~@":
                return try common_quote(r, "splice-unquote")
            case "^":
                let meta = try read_form(r)
                let form = try read_form(r)
                return make_list_from(kSymbolWithMeta, form, meta)
            case "@":
                let form = try read_form(r)
                return make_list_from(kSymbolDeref, form)
            default:
                return try read_atom(token)
        }
    }
    try throw_error("ran out of tokens -- possibly unbalanced ()'s")
}

func read_str(s: String) throws -> MalVal {
    let tokens = tokenizer(s)
    let reader = Reader(tokens)
    let obj = try read_form(reader)
    return obj
}
