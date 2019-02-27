import rl from './node_readline.js'
const readline = rl.readline
import { BlankException, read_str } from './reader'
import { pr_str } from './printer'

// read
const READ = str => read_str(str)

// eval
const EVAL = (ast, env) => ast

// print
const PRINT = exp => pr_str(exp, true)

// repl
const REP = str => PRINT(EVAL(READ(str), {}))

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
