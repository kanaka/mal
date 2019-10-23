import Foundation

public enum Reader {

    public static func read(_ str: String) throws -> Expr {
        return try Parsers.expr.orThrow(MalError("Can't parse")).parse(str)!
    }
}

private extension Parsers {

    static let expr = form <* endPattern

    static let form = oneOf(
        list,
        vector,
        hashmap,
        eString,
        number,
        symbol,
        keyword,
        sugar
    ).spacesAround()
    static let _form: Parser<Expr> = lazy(form)

    static let endPattern = oneOf(
        end,
        char(from: ")").zeroOrThrow(.unbalanced(unexpected: ")")),
        char(from: "]").zeroOrThrow(.unbalanced(unexpected: "]")),
        char(from: "}").zeroOrThrow(.unbalanced(unexpected: "}"))
    )

    static let digit = char(from: "0123456789")
    static let naturalNumber = digit.oneOrMore.map { Int(String($0)) }
    static let number = (optional(char(from: "-")) <*> naturalNumber).map(makeNumber)
    static func makeNumber(_ negative: Character?, value: Int) -> Expr {
        let factor = negative != nil ? -1 : 1
        return .number(value * factor)
    }

    static let list = ("(" *> _form.zeroOrMore.spacesAround() <* string(")").orThrow(.unbalanced(expected: ")"))).map(Expr.list)
    static let vector = ("[" *> _form.zeroOrMore.spacesAround() <* string("]").orThrow(.unbalanced(expected: "]"))).map(Expr.vector)

    static let hashmap = ("{" *> (hashmapKey <*> _form).zeroOrMore.spacesAround() <* string("}").orThrow(.unbalanced(expected: "}"))).map(makeExprHashmap)
    static func makeExprHashmap(_ xs: [(Expr, Expr)]) -> Expr {
        var dict: [String: Expr] = [:]
        for x in xs {
            guard case let .string(key) = x.0 else { fatalError() }
            dict[key] = x.1
        }
        return .hashmap(dict)
    }

    static let hashmapKey = oneOf(eString, keyword)

    static let stringContent = oneOf(
        string(excluding: "\\\""),
        string("\\\\").map { "\\" },
        string("\\\"").map { "\"" },
        string("\\n").map { "\n" },
        string("\\").map { "\\" }
    )

    static let eString = (
        "\"" *> stringContent.zeroOrMore <* string("\"").orThrow(.unbalanced(expected: "\""))
    ).map(makeExprString)

    static func makeExprString(_ xs: [String]) -> Expr {
        return .string(xs.joined())
    }

    static let symbolHead = char(excluding: "0123456789^`'\"#~@:%()[]{} \n\r\t,")
    static let symbolRest = oneOf(symbolHead, char(from: "0123456789."))
    static let symbol = name.map(Expr.symbol)

    static let name = (symbolHead <*> symbolRest.zeroOrMore).map { String($0) + String($1) }
    static let keyword = (":" *> name).map { Expr.string(String(keywordMagic) + $0) }

    static let quote = ("'" *> _form).map { Expr.list([.symbol("quote"), $0]) }
    static let quasiquote = ("`" *> _form).map { Expr.list([.symbol("quasiquote"), $0]) }
    static let spliceUnquote = ("~@" *> _form).map { Expr.list([.symbol("splice-unquote"), $0]) }
    static let unquote = ("~" *> _form).map { Expr.list([.symbol("unquote"), $0]) }
    static let deref = ("@" *> _form).map { Expr.list([.symbol("deref"), $0]) }
    static let meta = ("^" *> hashmap <*> _form).map { Expr.list([.symbol("with-meta"), $1, $0]) }

    static let sugar = oneOf(
        quote,
        quasiquote,
        spliceUnquote,
        unquote,
        deref,
        meta
    )
}

extension Parsers {

    static let whitespace = char(from: " \n\r\t,")
    static let comment = char(from: ";") <* char(excluding: "\r\n").zeroOrMore
    static let trash = oneOf(whitespace, comment)
}

extension Parser {

    func spacesAround() -> Parser {
        return (Parsers.trash.zeroOrMore *> self <* Parsers.trash.zeroOrMore)
    }
}
