import Foundation
import core

func read(_ s: String) throws -> Expr {
    return try Reader.read(s)
}

func eval(_ expr: Expr) throws -> Expr {
    return expr
}

func print(_ expr: Expr) -> String {
    return Expr.print(expr)
}

func rep(_ s: String) -> String {
    do {
        let expr = try read(s)
        let resExpr = try eval(expr)
        let resultStr = print(resExpr)
        return resultStr
    } catch {
        return error.localizedDescription
    }
}

while true {
    print("user> ", terminator: "")
    guard let s = readLine() else { break }
    print(rep(s))
}
