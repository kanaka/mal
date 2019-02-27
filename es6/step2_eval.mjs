import rl from './node_readline.js'
const readline = rl.readline
import { _list_Q } from './types'
import { BlankException, read_str } from './reader'
import { pr_str } from './printer'

// read
const READ = str => read_str(str)

// eval
const eval_ast = (ast, env) => {
    if (typeof ast === 'symbol') {
        if (ast in env) {
            return env[ast]
        } else {
            throw Error(`'${Symbol.keyFor(ast)}' not found`)
        }
    } else if (ast instanceof Array) {
        return ast.map(x => EVAL(x, env))
    } else if (ast instanceof Map) {
        let new_hm = new Map()
        ast.forEach((v, k) => new_hm.set(EVAL(k, env), EVAL(v, env)))
        return new_hm
    } else {
        return ast
    }
}

const EVAL = (ast, env) => {
    if (!_list_Q(ast)) { return eval_ast(ast, env) }
    if (ast.length === 0) { return ast }

    const [f, ...args] = eval_ast(ast, env)
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
