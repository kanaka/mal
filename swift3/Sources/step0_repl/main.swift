import Foundation

while true {
    print("user> ", terminator: "")
    let line = readLine(strippingNewline: true)
    if line == nil { break }
    if line == "" { continue }

    print("\(line!)")
}
