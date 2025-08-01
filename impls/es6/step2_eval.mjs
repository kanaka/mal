import rl from './node_readline.mjs'
const readline = rl.readline
import { _list_Q, Vector } from './types.mjs'
import { BlankException, read_str } from './reader.mjs'
import { pr_str } from './printer.mjs'

// read
const READ = str => read_str(str)

// eval
const EVAL = (ast, env) => {
    // console.log('EVAL:', pr_str(ast, true))

    if (typeof ast === 'symbol') {
        if (ast in env) {
            return env[ast]
        } else {
            throw Error(`'${Symbol.keyFor(ast)}' not found`)
        }
    } else if (ast instanceof Vector) {
        return ast.map(x => EVAL(x, env))
    } else if (ast instanceof Map) {
        let new_hm = new Map()
        ast.forEach((v, k) => new_hm.set(k, EVAL(v, env)))
        return new_hm
    } else if (!_list_Q(ast)) {
        return ast
    }

    if (ast.length === 0) { return ast }

    const [f, ...args] =ast.map(x => EVAL(x, env))
    return f(...args)
}

// print
const PRINT = exp => pr_str(exp, true)

// repl
var repl_env = {[Symbol.for('+')]: (a,b) => a+b,
                [Symbol.for('-')]: (a,b) => a-b,
                [Symbol.for('*')]: (a,b) => a*b,
                [Symbol.for('/')]: (a,b) => a/b}
const REP = str => PRINT(EVAL(READ(str), repl_env))

while (true) {
    let line = readline('user> ')
    if (line == null) break
    try {
        if (line) { console.log(REP(line)) }
    } catch (exc) {
        if (exc instanceof BlankException) { continue }
        if (exc instanceof Error) { console.warn(exc.stack) }
        else { console.warn(`Error: ${exc}`) }
    }
}
