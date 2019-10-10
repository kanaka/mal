import Foundation

public enum Expr {
    case number(Int)
    case string(String)
    case symbol(String)
    case keyword(String)
    case list([Expr])
    case vector([Expr])
    case hashmap([Expr: Expr])
}

extension Expr: Hashable {}
