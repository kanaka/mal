
import Foundation

func READ(_ input: String) throws -> MalData {
    return try read_str(input)
}

func EVAL(_ input: MalData) throws -> MalData {
    return input
}

func PRINT(_ input: MalData) -> String {
    return pr_str(input, print_readably: true)
}

@discardableResult func rep(_ input: String) throws -> String {
    return try PRINT(EVAL(READ(input)))
}


while true {
    print("user> ", terminator: "")
    if let input = readLine(strippingNewline: true) {
        guard input != "" else { continue }
        do {
            try print(rep(input))
        } catch let error as MalError {
            print(error.info())
        }
    } else {
        exit(0);
    }
}
