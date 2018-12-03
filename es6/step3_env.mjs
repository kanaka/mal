import rl from './node_readline.js'
const readline = rl.readline
import { _list_Q } from './types'
import { BlankException, read_str } from './reader'
import { pr_str } from './printer'
import { new_env, env_set, env_get } from './env'

// read
const READ = str => read_str(str)

// eval
const eval_ast = (ast, env) => {
    if (typeof ast === 'symbol') {
        return env_get(env, ast)
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
    //console.log('EVAL:', pr_str(ast, true))
    if (!_list_Q(ast)) { return eval_ast(ast, env) }
    if (ast.length === 0) { return ast }

    const [a0, a1, a2, a3] = ast
    switch (typeof a0 === 'symbol' ? Symbol.keyFor(a0) : Symbol(':default')) {
        case 'def!':
            return env_set(env, a1, EVAL(a2, env))
        case 'let*':
            let let_env = new_env(env)
            for (let i=0; i < a1.length; i+=2) {
                env_set(let_env, a1[i], EVAL(a1[i+1], let_env))
            }
            return EVAL(a2, let_env)
        default:
            let [f, ...args] = eval_ast(ast, env)
            return f(...args)
    }
}

// print
const PRINT = exp => pr_str(exp, true)

// repl
let repl_env = new_env()
env_set(repl_env, Symbol.for('+'), (a,b) => a+b)
env_set(repl_env, Symbol.for('-'), (a,b) => a-b)
env_set(repl_env, Symbol.for('*'), (a,b) => a*b)
env_set(repl_env, Symbol.for('/'), (a,b) => a/b)
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
