import { readline } from './node_readline'

// read
const READ = (str) => str

// eval
const EVAL = (ast, env) => ast

// print
const PRINT = (exp) => exp

// repl
const REP = (str) => PRINT(EVAL(READ(str), {}))

while (true) {
    let line = readline('user> ')
    if (line == null) break
    if (line) { console.log(REP(line)); }
}
