import promptImp = require('prompt-sync')
import { readStr } from './reader'
import { pr_str } from './printer'
import { MalType } from './types'
const prompt = promptImp({sigint: true})

function READ(input: string): MalType {
    return readStr(input)
}

function EVAL(mal: MalType): MalType {
    return mal
}

function PRINT(mal: MalType): string {
    return pr_str(mal)
}

function REP(input: string): string {
    const r = READ(input)
    const e = EVAL(r)
    const p = PRINT(e)
    return p
}

while (true) {

    let line: string = prompt('user> ')

    if (line == null) {
        break;
    }
    if (line === "") {
        continue;
    }

    console.log(REP(line))
}