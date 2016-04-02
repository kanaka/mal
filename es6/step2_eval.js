import { readline } from './node_readline'
import { _symbol, _symbol_Q, _list_Q, _vector, _vector_Q,
         _hash_map_Q } from './types'
import { BlankException, read_str } from './reader'
import { pr_str } from './printer'

// read
const READ = (str) => read_str(str)

// eval
const eval_ast = (ast, env) => {
    if (_symbol_Q(ast)) {
        if (ast in env) {
            return env[ast]
        } else {
            throw Error(`'${Symbol.keyFor(ast)}' not found`)
        }
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
    if (!_list_Q(ast)) { return eval_ast(ast, env) }
    if (ast.length === 0) { return ast }

    const [f, ...args] = eval_ast(ast, env)
    return f(...args)
}

// print
const PRINT = (exp) => pr_str(exp, true)

// repl
var repl_env = {[_symbol('+')]: (a,b) => a+b,
                [_symbol('-')]: (a,b) => a-b,
                [_symbol('*')]: (a,b) => a*b,
                [_symbol('/')]: (a,b) => a/b}
const REP = (str) => PRINT(EVAL(READ(str), repl_env))

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
