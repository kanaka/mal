import Foundation

public enum Reader {

    public static func read(_ str: String) throws -> Expr {
        return try Parsers.expr.orThrow(MalError.reader()).parse(str)!
    }
}

private extension Parsers {

    static let expr = form <* endPattern

    static let endPattern = oneOf(
        end,
        char(from: ")").zeroOrThrow(.unbalanced(unexpected: ")")),
        char(from: "]").zeroOrThrow(.unbalanced(unexpected: "]")),
        char(from: "}").zeroOrThrow(.unbalanced(unexpected: "}"))
    )

    static let form = oneOf(
        list,
        vector,
        hashmap,
        atom,
        readerMacros
    ).ignoreAround()

    static let _form: Parser<Expr> = lazy(form)

    static let atom = oneOf(
        malString,
        number,
        null,
        bool,
        symbol,
        keyword
    )

    static let list = ("(" *> _form.zeroOrMore.ignoreAround() <* string(")").orThrow(.unbalanced(expected: ")"))).map { Expr.list($0) }
    static let vector = ("[" *> _form.zeroOrMore.ignoreAround() <* string("]").orThrow(.unbalanced(expected: "]"))).map { Expr.vector($0) }

    // MARK: - Hashmap

    static let hashmap = ("{" *> (hashmapKey <*> _form).zeroOrMore.ignoreAround() <* string("}").orThrow(.unbalanced(expected: "}"))).map(makeHashmap)
    static func makeHashmap(_ xs: [(Expr, Expr)]) -> Expr {
        var dict: [String: Expr] = [:]
        for x in xs {
            guard case let .string(key) = x.0 else { fatalError() }
            dict[key] = x.1
        }
        return .hashmap(dict)
    }

    static let hashmapKey = oneOf(malString, keyword)

    // MARK: - Number

    static let number = (optional(char(from: "-")) <*> naturalNumber).map(makeNumber)
    static func makeNumber(_ negative: Character?, value: Int) -> Expr {
        let factor = negative != nil ? -1 : 1
        return .number(value * factor)
    }

    // MARK: - String

    static let stringContent = oneOf(
        string(excluding: "\\\""),
        string("\\\\").map { "\\" },
        string("\\\"").map { "\"" },
        string("\\n").map { "\n" },
        string("\\").map { "\\" }
    )

    static let malString = ("\"" *> stringContent.zeroOrMore <* string("\"").orThrow(.unbalanced(expected: "\""))).map(makeMalString)
    static func makeMalString(_ xs: [String]) -> Expr {
        return .string(xs.joined())
    }

    // MARK: - Keyword

    static let keyword = (":" *> name).map { Expr.string(String(keywordMagic) + $0) }

    // MARK: - Symbol

    static let symbolHead = char(excluding: "0123456789^`'\"#~@:%()[]{} \n\r\t,")
    static let symbolRest = oneOf(symbolHead, char(from: "0123456789."))
    static let name = (symbolHead <*> symbolRest.zeroOrMore).map { String($0) + String($1) }
    static let symbol = name.map(Expr.symbol)

    // MARK: - Bool

    static let bool = name.map(makeBool)
    static func makeBool(_ s: String) -> Expr? {
        switch s {
        case "true": return .bool(true)
        case "false": return .bool(false)
        default: return nil
        }
    }

    // MARK: - Null

    static let null = name.map(makeNull)
    static func makeNull(_ s: String) -> Expr? {
        return s == "nil" ? .null : nil
    }

    // MARK: - Reader macros

    static let quote = ("'" *> _form).readerMacros("quote")
    static let quasiquote = ("`" *> _form).readerMacros("quasiquote")
    static let spliceUnquote = ("~@" *> _form).readerMacros("splice-unquote")
    static let unquote = ("~" *> _form).readerMacros("unquote")
    static let deref = ("@" *> _form).readerMacros("deref")
    static let meta = ("^" *> hashmap <*> _form).map { Expr.list([.symbol("with-meta"), $1, $0]) }


    static let readerMacros = oneOf(
        quote,
        quasiquote,
        spliceUnquote,
        unquote,
        deref,
        meta
    )

    // MARK: - Ignore

    static let whitespace = char(from: " \n\r\t,")
    static let comment = char(from: ";") <* char(excluding: "\n\r").zeroOrMore
    static let ignore = oneOf(whitespace, comment)
}

extension Parser {

    func ignoreAround() -> Parser {
        return (Parsers.ignore.zeroOrMore *> self <* Parsers.ignore.zeroOrMore)
    }
}

extension Parser where A == Expr {
    func readerMacros(_ s: String) -> Parser<Expr> {
        return map { Expr.list([.symbol(s), $0]) }
    }
}
