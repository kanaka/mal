import { readline } from "./node_readline";

import { Node, MalType, MalNumber, MalList, MalVector, MalHashMap, MalSymbol, MalFunction, isSeq } from "./types";
import { Env } from "./env";
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
                        throw new Error(`unexpected toke type: ${key.type}, expected: symbol`);
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
                        throw new Error(`unexpected toke type: ${pairs.type}, expected: list or vector`);
                    }
                    const list = pairs.list;
                    for (let i = 0; i < list.length; i += 2) {
                        const key = list[i];
                        const value = list[i + 1];
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

replEnv.set(MalSymbol.get("+"), MalFunction.fromBootstrap((a?: MalNumber, b?: MalNumber) => new MalNumber(a!.v + b!.v)));
replEnv.set(MalSymbol.get("-"), MalFunction.fromBootstrap((a?: MalNumber, b?: MalNumber) => new MalNumber(a!.v - b!.v)));
replEnv.set(MalSymbol.get("*"), MalFunction.fromBootstrap((a?: MalNumber, b?: MalNumber) => new MalNumber(a!.v * b!.v)));
replEnv.set(MalSymbol.get("/"), MalFunction.fromBootstrap((a?: MalNumber, b?: MalNumber) => new MalNumber(a!.v / b!.v)));

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
