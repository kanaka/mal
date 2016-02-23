import Foundation

func READ(str: String) throws -> MalVal {
    return try read_str(str)
}

func EVAL(ast: MalVal, _ env: String) throws -> MalVal {
    return ast
}

func PRINT(exp: MalVal) -> String {
    return pr_str(exp, true)
}


func rep(str:String) throws -> String {
    return PRINT(try EVAL(try READ(str), ""))
}

while true {
    print("user> ", terminator: "")
    let line = readLine(stripNewline: true)
    if line == nil { break }
    if line == "" { continue }

    do {
        print(try rep(line!))
    } catch (MalError.Reader(let msg)) {
        print("Error: \(msg)")
    }
}
