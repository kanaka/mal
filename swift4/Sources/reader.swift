
import Foundation

struct Reader {
    let tokens: [String]
    var position = 0
    
    init(tokens: [String]) {
        self.tokens = tokens
    }
    
    mutating func next() -> String? {
        guard tokens.indices.contains(position) else {
            return nil
        }
        position += 1
        return tokens[position - 1]
    }
    
    func peak() -> String? {
        guard tokens.indices.contains(position) else {
            return nil
        }
        return tokens[position]
    }
    
    mutating func pass() {
        guard tokens.indices.contains(position) else {
            return
        }
        position += 1
    }
    
    mutating func read_form() throws -> MalData {
        guard let token = peak() else { throw MalError.Error }
        switch token {
        case "(", "[", "{":
            return try read_list(startWith: token)
        case "'", "`", "~", "~@", "@":
            let readerMacros = ["'": "quote",
                                "`": "quasiquote",
                                "~": "unquote",
                                "~@": "splice-unquote",
                                "@": "deref"]
            pass() // pass the mark
            return try [Symbol(readerMacros[token]!), read_form()]
        case "^":
            pass() // pass the mark
            let meta = try read_form()
            return try [Symbol("with-meta"), read_form(), meta]
        default:
            return try read_atom()
        }
    }
    
    
    mutating func read_list(startWith leftParen: String) throws -> MalData {
        pass() // pass the left paren
        defer {
            pass() // pass the right paren
        }
        
        var list: [MalData] = []
        while ![")", "]", "}"].contains(peak())  {
            guard peak() != nil else {
                throw MalError.ParensMismatch
            }
            list.append(try read_form())
        }
        
        switch (leftParen, peak()) {
        case ("(", ")"):
            return list
        case ("[", "]"):
            return Vector(list)
        case ("{", "}"):
            var hashMap: [String: MalData] = [:]
            for index in stride(from: 0, to: list.count, by: 2) {
                guard list[index] is String, index+1 < list.count else { throw MalError.Error }
                hashMap.updateValue(list[index+1], forKey: list[index] as! String)
            }
            return hashMap
        default:
            throw MalError.ParensMismatch
        }
    }
    
    mutating func read_atom() throws -> MalData {
        let token = next()!
        let regexInt = "^-?[0-9]+$"
        let regexString = "\"(?:\\\\.|[^\\\\\"])*\""
        let regexStringUnbalanced = "\"(?:\\\\.|[^\\\\\"])*"
        let regexKeyword = "^:"
        func match(string: String, regex: String) -> Bool {
            return token.range(of: regex, options: .regularExpression, range: token.startIndex..<token.endIndex) != nil
        }
        switch token {
        case let token where match(string: token, regex: regexInt):
            return Int(token)!
        case let token where match(string: token, regex: regexKeyword):
            let firstChar = token.startIndex...token.startIndex
            return token.replacingCharacters(in: firstChar, with: "\u{029E}")
        case let token where match(string: token, regex: regexString):
            let stripped = token.dropFirst().dropLast()
            return stripped.replacingOccurrences(of: "\\\\", with: "\u{029E}")
                .replacingOccurrences(of: "\\\"", with: "\"")
                .replacingOccurrences(of: "\\n", with: "\n")
                .replacingOccurrences(of: "\u{029E}", with: "\\")
        case let token where match(string: token, regex: regexStringUnbalanced):
            throw MalError.QuotationMarkMismatch
        case "true":
            return true
        case "false":
            return false
        case "nil":
            return Nil()
        default:
            return Symbol(token)
        }
    }
}

func tokenizer(_ input: String) -> [String] {
    guard let regex = try? NSRegularExpression(pattern: "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:[\\\\].|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}()'\"`@,;]+)", options: .useUnixLineSeparators)
        else { return [] }
    let matches = regex.matches(in: input, range: NSMakeRange(0, input.count))
    
    return matches.map { match in
        String(input[Range(match.range(at: 1), in: input)!])
    }.filter { token in
        !token.hasPrefix(";") && !token.isEmpty }
}


func read_str(_ input: String) throws -> MalData {
    let tokens = tokenizer(input)
    guard tokens.count>0 else {
        throw MalError.EmptyData
    }
    var reader = Reader(tokens: tokens)
    return try reader.read_form()
}





