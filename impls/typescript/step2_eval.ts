import promptImp = require('prompt-sync')
import { readStr } from './reader'
import { pr_str } from './printer'
import { MalType, MalNumber, MalList, MalFunc, MalTypes, MalSymbol } from './types'
const prompt = promptImp({sigint: true})

interface MalEnvironment {
    [key: string]: MalFunc;
}

function READ(input: string): MalType {
    return readStr(input)
}

function EVAL(mal: MalType, env: MalEnvironment): MalType {
    return eval_ast(mal, env)
}

function PRINT(mal: MalType): string {
    return pr_str(mal)
}

function REP(input: string): string {
    const r = READ(input)
    console.log(pr_str(r))
    const e = EVAL(r, repl_env)
    const p = PRINT(e)
    return p
}

// TODO: add support for more params
const add = new MalFunc((a: MalNumber, b: MalNumber) => new MalNumber(a.value + b.value))
const sub = new MalFunc((a: MalNumber, b: MalNumber) => new MalNumber(a.value - b.value))
const multiply = new MalFunc((a: MalNumber, b: MalNumber) => new MalNumber(a.value * b.value))
const divide = new MalFunc((a: MalNumber, b: MalNumber) => new MalNumber(a.value / b.value))

const repl_env: MalEnvironment = {
    '+': add, 
    '-': sub,
    "*": multiply,
    "/": divide
}


function eval_ast(ast: MalType, env: MalEnvironment): MalType {
    switch (ast.type) {
        case MalTypes.Number:
            return ast
        case MalTypes.Symbol:
            const symbol = (ast as MalSymbol).value
            const func = env[symbol]
            if (func === undefined) throw new Error("Unrecognized symbol: " + symbol)
            return func
        case MalTypes.List:
            const list = (ast as MalList).list
            const evaluated_list = list.map((malItem) => eval_ast(malItem, env))
            const f = evaluated_list[0] as MalFunc
            const args = new MalList(evaluated_list.slice(1, evaluated_list.length))
            const a = f.apply(args)
            return a
    }
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