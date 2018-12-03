import { readline } from "./node_readline";

import { Node, MalType, MalNil, MalList, MalVector, MalHashMap, MalFunction, isAST, isSeq } from "./types";
import { Env } from "./env";
import * as core from "./core";
import { readStr } from "./reader";
import { prStr } from "./printer";

// READ
function read(str: string): MalType {
    return readStr(str);
}

function evalAST(ast: MalType, env: Env): MalType {
    switch (ast.type) {
        case Node.Symbol:
            const f = env.get(ast);
            if (!f) {
                throw new Error(`unknown symbol: ${ast.v}`);
            }
            return f;
        case Node.List:
            return new MalList(ast.list.map(ast => evalMal(ast, env)));
        case Node.Vector:
            return new MalVector(ast.list.map(ast => evalMal(ast, env)));
        case Node.HashMap:
            const list: MalType[] = [];
            for (const [key, value] of ast.entries()) {
                list.push(key);
                list.push(evalMal(value, env));
            }
            return new MalHashMap(list);
        default:
            return ast;
    }
}

// EVAL
function evalMal(ast: MalType, env: Env): MalType {
    if (ast.type !== Node.List) {
        return evalAST(ast, env);
    }
    if (ast.list.length === 0) {
        return ast;
    }
    const first = ast.list[0];
    switch (first.type) {
        case Node.Symbol:
            switch (first.v) {
                case "def!": {
                    const [, key, value] = ast.list;
                    if (key.type !== Node.Symbol) {
                        throw new Error(`unexpected token type: ${key.type}, expected: symbol`);
                    }
                    if (!value) {
                        throw new Error(`unexpected syntax`);
                    }
                    return env.set(key, evalMal(value, env));
                }
                case "let*": {
                    let letEnv = new Env(env);
                    const pairs = ast.list[1];
                    if (!isSeq(pairs)) {
                        throw new Error(`unexpected token type: ${pairs.type}, expected: list or vector`);
                    }
                    for (let i = 0; i < pairs.list.length; i += 2) {
                        const key = pairs.list[i];
                        const value = pairs.list[i + 1];
                        if (key.type !== Node.Symbol) {
                            throw new Error(`unexpected token type: ${key.type}, expected: symbol`);
                        }
                        if (!key || !value) {
                            throw new Error(`unexpected syntax`);
                        }

                        letEnv.set(key, evalMal(value, letEnv));
                    }
                    return evalMal(ast.list[2], letEnv);
                }
                case "do": {
                    const [, ...list] = ast.list;
                    const ret = evalAST(new MalList(list), env);
                    if (!isSeq(ret)) {
                        throw new Error(`unexpected return type: ${ret.type}, expected: list or vector`);
                    }
                    return ret.list[ret.list.length - 1];
                }
                case "if": {
                    const [, cond, thenExpr, elseExrp] = ast.list;
                    const ret = evalMal(cond, env);
                    let b = true;
                    if (ret.type === Node.Boolean && !ret.v) {
                        b = false;
                    } else if (ret.type === Node.Nil) {
                        b = false;
                    }
                    if (b) {
                        return evalMal(thenExpr, env);
                    } else if (elseExrp) {
                        return evalMal(elseExrp, env);
                    } else {
                        return MalNil.instance;
                    }
                }
                case "fn*": {
                    const [, args, binds] = ast.list;
                    if (!isSeq(args)) {
                        throw new Error(`unexpected return type: ${args.type}, expected: list or vector`);
                    }
                    const symbols = args.list.map(param => {
                        if (param.type !== Node.Symbol) {
                            throw new Error(`unexpected return type: ${param.type}, expected: symbol`);
                        }
                        return param;
                    });
                    return MalFunction.fromBootstrap((...fnArgs: MalType[]) => {
                        return evalMal(binds, new Env(env, symbols, fnArgs));
                    });
                }
            }
    }
    const result = evalAST(ast, env);
    if (!isSeq(result)) {
        throw new Error(`unexpected return type: ${result.type}, expected: list or vector`);
    }
    const [f, ...args] = result.list;
    if (f.type !== Node.Function) {
        throw new Error(`unexpected token: ${f.type}, expected: function`);
    }
    return f.func(...args);
}

// PRINT
function print(exp: MalType): string {
    return prStr(exp);
}

const replEnv = new Env();
function rep(str: string): string {
    return print(evalMal(read(str), replEnv));
}

// core.EXT: defined using Racket
core.ns.forEach((value, key) => {
    replEnv.set(key, value);
});

// core.mal: defined using the language itself
rep("(def! not (fn* (a) (if a false true)))");

while (true) {
    const line = readline("user> ");
    if (line == null) {
        break;
    }
    if (line === "") {
        continue;
    }
    try {
        console.log(rep(line));
    } catch (e) {
        if (isAST(e)) {
            console.error("Error:", prStr(e));
        } else {
            const err: Error = e;
            console.error("Error:", err.message);
        }
    }
}
