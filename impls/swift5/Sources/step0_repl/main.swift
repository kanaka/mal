import Foundation

func READ(_ s: String) -> String {
    return s
}

func EVAL(_ s: String) -> String {
    return s
}

func PRINT(_ s: String) -> String {
    return s
}

func rep(_ s: String) -> String {
    return PRINT(EVAL(READ(s)))
}

while true {
    print("user> ", terminator: "")
    guard let s = readLine() else { break }
    print(rep(s))
}
