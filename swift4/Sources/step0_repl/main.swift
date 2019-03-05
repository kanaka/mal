//
//  main.swift
//  swift4
//
//  Created by LuLouie on 2019/1/6.
//  Copyright © 2019 llvm.xyz. All rights reserved.
//

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
    }
}
