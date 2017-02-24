import { readline } from "./node_readline";

import { MalType, MalNumber, MalList, MalVector, MalHashMap, MalSymbol, MalFunction } from "./types";
import { Env } from "./env";
import { readStr } from "./reader";
import { prStr } from "./printer";

function read(str: string): MalType {
    return readStr(str);
}

function evalAST(ast: MalType, env: Env): MalType {
    switch (ast.type) {
        case "symbol":
            const f = env.get(ast);
            if (!f) {
                throw new Error(`unknown symbol: ${ast.v}`);
            }
            return f;
        case "list":
            return new MalList(ast.list.map(ast => evalSexp(ast, env)));
        case "vector":
            return new MalVector(ast.list.map(ast => evalSexp(ast, env)));
        case "hash-map":
            const list: MalType[] = [];
            for (const [key, value] of ast.entries()) {
                list.push(key);
                list.push(evalSexp(value, env));
            }
            return new MalHashMap(list);
        default:
            return ast;
    }
}

function evalSexp(ast: MalType, env: Env): MalType {
    if (ast.type !== "list") {
        return evalAST(ast, env);
    }
    if (ast.list.length === 0) {
        return ast;
    }
    const first = ast.list[0];
    switch (first.type) {
        case "symbol":
            switch (first.v) {
                case "def!": {
                    const [, key, value] = ast.list;
                    if (key instanceof MalSymbol === false) {
                        throw new Error(`unexpected toke type: ${key.type}, expected: symbol`);
                    }
                    if (!value) {
                        throw new Error(`unexpected syntax`);
                    }
                    return env.set(key as MalSymbol, evalSexp(value, env))
                }
                case "let*": {
                    let letEnv = new Env(env);
                    const pairs = ast.list[1];
                    if (pairs instanceof MalList === false && pairs instanceof MalVector === false) {
                        throw new Error(`unexpected toke type: ${pairs.type}, expected: list or vector`);
                    }
                    const list = (pairs as (MalList | MalVector)).list;
                    for (let i = 0; i < list.length; i += 2) {
                        const key = list[i];
                        const value = list[i + 1];
                        if (!key || !value) {
                            throw new Error(`unexpected syntax`);
                        }

                        letEnv.set(key as MalSymbol, evalSexp(value, letEnv));
                    }
                    return evalSexp(ast.list[2], letEnv);
                }
            }
    }
    const result = evalAST(ast, env) as MalList;
    const [f, ...args] = result.list;
    if (!MalFunction.is(f)) {
        throw new Error(`unexpected token: ${f.type}, expected: function`);
    }
    return f.func(...args);
}

function print(exp: MalType): string {
    return prStr(exp);
}

const replEnv = new Env();
replEnv.set(MalSymbol.get("+"), MalFunction.fromBootstrap((a?: MalNumber, b?: MalNumber) => new MalNumber(a!.v + b!.v)));
replEnv.set(MalSymbol.get("-"), MalFunction.fromBootstrap((a?: MalNumber, b?: MalNumber) => new MalNumber(a!.v - b!.v)));
replEnv.set(MalSymbol.get("*"), MalFunction.fromBootstrap((a?: MalNumber, b?: MalNumber) => new MalNumber(a!.v * b!.v)));
replEnv.set(MalSymbol.get("/"), MalFunction.fromBootstrap((a?: MalNumber, b?: MalNumber) => new MalNumber(a!.v / b!.v)));

function rep(str: string): string {
    return print(evalSexp(read(str), replEnv));
}

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
        const err: Error = e;
        console.error(err.message);
    }
}
