import { readline } from './node_readline';
import { Sym, _list_Q } from './types';
import { BlankException, read_str } from './reader';
import { pr_str } from './printer';
import { new_env } from './env';

// read
const READ = (str) => read_str(str);

// eval
const eval_ast = (ast, env) => {
    if (ast instanceof Sym) {
        return env.get(ast)
    } else if (_list_Q(ast)) {
        return ast.map((x) => EVAL(x, env));
    } else {
        return ast;
    }
}

const EVAL = (ast, env) => {
    if (!_list_Q(ast)) { return eval_ast(ast, env) }

    let [{ name: a0sym }, a1, a2, a3] = ast;
    switch (a0sym) {
        case 'def!': 
            return env.set(a1, EVAL(a2, env));
        case 'let*':
            let let_env = new_env(env);
            for (let i=0; i < a1.length; i+=2) {
                let_env.set(a1[i], EVAL(a1[i+1], let_env));
            }
            return EVAL(a2, let_env);
        default:
            let [f, ...args] = eval_ast(ast, env);
            return f(...args);
    }
}

// print
const PRINT = (exp) => pr_str(exp, true);

// repl
let repl_env = new_env();
repl_env.set(new Sym('+'), (a,b) => a+b);
repl_env.set(new Sym('-'), (a,b) => a-b);
repl_env.set(new Sym('*'), (a,b) => a*b);
repl_env.set(new Sym('/'), (a,b) => a/b);
const REP = (str) => PRINT(EVAL(READ(str), repl_env));

while (true) {
    let line = readline("user> ");
    if (line == null) break;
    try {
        if (line) { console.log(REP(line)); }
    } catch (exc) {
        if (exc instanceof BlankException) { continue; }
        if (exc.stack) { console.log(exc.stack); }
        else           { console.log("Error: " + exc); }
    }
}
