import rl from './node_readline.mjs'
const readline = rl.readline
import { _list_Q, Vector } from './types.mjs'
import { BlankException, read_str } from './reader.mjs'
import { pr_str } from './printer.mjs'
import { new_env, env_set, env_get } from './env.mjs'
import { core_ns } from './core.mjs'

// read
const READ = str => read_str(str)

// eval
const dbgevalsym = Symbol.for("DEBUG-EVAL")

const EVAL = (ast, env) => {
    if (dbgevalsym in env) {
        const dbgeval = env_get(env, dbgevalsym)
        if (dbgeval !== null && dbgeval !== false) {
            console.log('EVAL:', pr_str(ast, true))
        }
    }

    if (typeof ast === 'symbol') {
        return env_get(env, ast)
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
        case 'do':
            return ast.slice(1).map(x => EVAL(x, env))[ast.length-2]
        case 'if':
            let cond = EVAL(a1, env)
            if (cond === null || cond === false) {
                return typeof a3 !== 'undefined' ? EVAL(a3, env) : null
            } else {
                return EVAL(a2, env)
            }
        case 'fn*':
            return (...args) => EVAL(a2, new_env(env, a1, args))
        default:
            const [f, ...args] = ast.map(x => EVAL(x, env))
            return f(...args)
    }
}

// print
const PRINT = exp => pr_str(exp, true)

// repl
let repl_env = new_env()
const REP = str => PRINT(EVAL(READ(str), repl_env))

// core.EXT: defined using ES6
for (let [k, v] of core_ns) { env_set(repl_env, Symbol.for(k), v) }

// core.mal: defined using language itself
REP('(def! not (fn* (a) (if a false true)))')

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
