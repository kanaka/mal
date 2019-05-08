
import Foundation

func READ(_ input:String) -> String {
    return input
}

func EVAL(_ input:String) -> String {
    return input
}

func PRINT(_ input:String) -> String {
    return input
}

@discardableResult func rep(_ input:String) -> String {
    return PRINT(EVAL(READ(input)))
}

while true {
    print("user> ", terminator: "")
    if let input = readLine(strippingNewline: true) {
        print(rep(input))
    } else {
        exit(0);
    }
}
