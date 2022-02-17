import promptImp = require('prompt-sync')
import { readStr } from './reader'
import { pr_str } from './printer'
import { MalType, MalNumber, MalList, MalFunc, MalTypes, MalSymbol } from './types'
import { Env } from './env'
const prompt = promptImp({sigint: true})

interface MalEnvironment {
    [key: string]: MalFunc;
}

function READ(input: string): MalType {
    return readStr(input)
}

function EVAL(mal: MalType, env: Env): MalType {
    return eval_ast(mal, env)
}

function PRINT(mal: MalType): string {
    return pr_str(mal)
}

function REP(input: string, repl_env: Env): string {
    const r = READ(input)
    const e = EVAL(r, repl_env)
    const p = PRINT(e)
    return p
}

function eval_ast(ast: MalType, repl_env: Env): MalType {
    switch(ast.type) {
        case MalTypes.Number:
        case MalTypes.Function:
            return ast
        case MalTypes.Symbol:
            // Assumes that value for the symbol in the env is "resolved"
            // i.e. it is either an atom or function
            const symbol = ast as MalSymbol
            return repl_env.get(symbol)
        case MalTypes.List:
            const list = (ast as MalList).list
            if (list[0].type === MalTypes.Symbol && (list[0] as MalSymbol).value === "def!") {
                const symbol = list[1] as MalSymbol
                const value = eval_ast(list[2], repl_env)
                def(symbol, value, repl_env)
                return value
            } else if (list[0].type === MalTypes.Symbol && (list[0] as MalSymbol).value === "let*") {
                const newEnv = new Env(repl_env)
                const bindingList = (list[1] as MalList).list
                for (let i = 0; i < bindingList.length - 1; i += 2) {
                    const key = bindingList[i] as MalSymbol
                    const value = eval_ast(bindingList[i+1], newEnv)
                    newEnv.set(key, value)
                }
                const expr = list[2]
                return eval_ast(expr, newEnv)
            } else {
                // non-special forms
                // evaluate each item in array, then apply the function to the args
                const evaluated_list = list.map((malItem) => eval_ast(malItem, repl_env))
                const func = evaluated_list[0] as MalFunc
                const args = new MalList(evaluated_list.slice(1, evaluated_list.length))
                return func.apply(args)
            }
        default: 
            throw new Error("MalType: " + ast.type + " not found.")
    }
}

function def(key: MalSymbol, value: MalType, env: Env): void {
    env.set(key, value)
}


function createEnv(): Env {
    // TODO: add support for more params
    const add = new MalFunc((a: MalNumber, b: MalNumber) => new MalNumber(a.value + b.value))
    const sub = new MalFunc((a: MalNumber, b: MalNumber) => new MalNumber(a.value - b.value))
    const multiply = new MalFunc((a: MalNumber, b: MalNumber) => new MalNumber(a.value * b.value))
    const divide = new MalFunc((a: MalNumber, b: MalNumber) => new MalNumber(a.value / b.value))
    const env = new Env()
    env.set(new MalSymbol("+"), add)
    env.set(new MalSymbol("-"), sub)
    env.set(new MalSymbol("*"), multiply)
    env.set(new MalSymbol("/"), divide)
    return env
}

export function main() {
    const _env = createEnv()
    while (true) {
        let line: string = prompt('user> ')
        if (line == null) {
            break;
        }
        if (line === "") {
            continue;
        }
        console.log(REP(line, _env))
    }
}

// For testing
module.exports = {
    main,
    READ,
    EVAL,
    PRINT,
    REP,
    eval_ast,
    createEnv
}

