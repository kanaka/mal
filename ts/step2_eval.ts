import { readline } from "./node_readline";

import { MalType, MalNumber, MalList, MalVector, MalHashMap, MalFunction } from "./types";
import { readStr } from "./reader";
import { prStr } from "./printer";

function read(str: string): MalType {
    return readStr(str);
}

interface MalEnvironment {
    [key: string]: MalFunction;
}

function evalAST(ast: MalType, env: MalEnvironment): MalType {
    switch (ast.type) {
        case "symbol":
            const f = env[ast.v];
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
            for (const [key, value] of ast.map) {
                list.push(key);
                list.push(evalSexp(value, env));
            }
            return new MalHashMap(list);
        default:
            return ast;
    }
}

function evalSexp(ast: MalType, env: MalEnvironment): MalType {
    if (ast.type !== "list") {
        return evalAST(ast, env);
    }
    if (ast.list.length === 0) {
        return ast;
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

const replEnv: MalEnvironment = {
    "+": MalFunction.fromBootstrap((a?: MalNumber, b?: MalNumber) => new MalNumber(a!.v + b!.v)),
    "-": MalFunction.fromBootstrap((a?: MalNumber, b?: MalNumber) => new MalNumber(a!.v - b!.v)),
    "*": MalFunction.fromBootstrap((a?: MalNumber, b?: MalNumber) => new MalNumber(a!.v * b!.v)),
    "/": MalFunction.fromBootstrap((a?: MalNumber, b?: MalNumber) => new MalNumber(a!.v / b!.v)),
};
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
