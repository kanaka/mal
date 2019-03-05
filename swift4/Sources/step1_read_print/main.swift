//
//  main.swift
//  swift4
//
//  Created by LuLouie on 2019/1/6.
//  Copyright © 2019 llvm.xyz. All rights reserved.
//

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
    }
}
