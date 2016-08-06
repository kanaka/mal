import { readline } from './node_readline'
import { _symbol, _symbol_Q, _list_Q, _vector, _vector_Q,
         _hash_map_Q, _malfunc, _malfunc_Q } from './types'
import { BlankException, read_str } from './reader'
import { pr_str } from './printer'
import { new_env, env_set, env_get } from './env'
import { core_ns } from './core'

// read
const READ = (str) => read_str(str)

// eval
const eval_ast = (ast, env) => {
    if (_symbol_Q(ast)) {
        return env_get(env, ast)
    } else if (_list_Q(ast)) {
        return ast.map((x) => EVAL(x, env))
    } else if (_vector_Q(ast)) {
        return _vector(...ast.map((x) => EVAL(x, env)))
    } else if (_hash_map_Q(ast)) {
        let new_hm = new Map()
        for (let [k, v] of ast) {
            new_hm.set(EVAL(k, env), EVAL(v, env))
        }
        return new_hm
    } else {
        return ast
    }
}

const EVAL = (ast, env) => {
  while (true) {
    //console.log('EVAL:', pr_str(ast, true))
    if (!_list_Q(ast)) { return eval_ast(ast, env) }
    if (ast.length === 0) { return ast }

    const [a0, a1, a2, a3] = ast
    const a0sym = _symbol_Q(a0) ? Symbol.keyFor(a0) : Symbol(':default')
    switch (a0sym) {
        case 'def!': 
            return env_set(env, a1, EVAL(a2, env))
        case 'let*':
            let let_env = new_env(env)
            for (let i=0; i < a1.length; i+=2) {
                env_set(let_env, a1[i], EVAL(a1[i+1], let_env))
            }
            env = let_env
            ast = a2
            break; // continue TCO loop
        case 'do':
            eval_ast(ast.slice(1,-1), env)
            ast = ast[ast.length-1]
            break; // continue TCO loop
        case 'if':
            let cond = EVAL(a1, env)
            if (cond === null || cond === false) {
                ast = (typeof a3 !== 'undefined') ? a3 : null
            } else {
                ast = a2
            }
            break; // continue TCO loop
        case 'fn*':
            return _malfunc((...args) => EVAL(a2, new_env(env, a1, args)),
                    a2, env, a1)
        default:
            let [f, ...args] = eval_ast(ast, env)
            if (_malfunc_Q(f)) {
                env = new_env(f.env, f.params, args)
                ast = f.ast
                break; // continue TCO loop
            } else {
                return f(...args)
            }
    }
  }
}

// print
const PRINT = (exp) => pr_str(exp, true)

// repl
let repl_env = new_env()
const REP = (str) => PRINT(EVAL(READ(str), repl_env))

// core.EXT: defined using ES6
for (let [k, v] of core_ns) { env_set(repl_env, _symbol(k), v) }
env_set(repl_env, _symbol('eval'), a => EVAL(a, repl_env))
env_set(repl_env, _symbol('*ARGV*'), [])

// core.mal: defined using language itself
REP('(def! not (fn* (a) (if a false true)))')
REP('(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) ")")))))')

if (process.argv.length > 2) { 
    env_set(repl_env, _symbol('*ARGV*'), process.argv.slice(3))
    REP(`(load-file "${process.argv[2]}")`)
    process.exit(0)
}


while (true) {
    let line = readline('user> ')
    if (line == null) break
    try {
        if (line) { console.log(REP(line)); }
    } catch (exc) {
        if (exc instanceof BlankException) { continue; }
        if (exc.stack) { console.log(exc.stack); }
        else           { console.log(`Error: ${exc}`); }
    }
}
