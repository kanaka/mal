import Foundation

public let keywordMagic: Character = "\u{029E}"

public enum Expr {
    case number(Int)
    case string(String)
    case symbol(String)
    case list([Expr])
    case vector([Expr])
    case hashmap([String: Expr])
    case function(Func)
}

public struct Func {
    public let run: ([Expr]) throws -> Expr

    public init(run: @escaping ([Expr]) throws -> Expr) {
        self.run = run
    }
}
