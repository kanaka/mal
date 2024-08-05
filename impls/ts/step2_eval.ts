import { readline } from "./node_readline";

import { Node, MalType, MalNumber, MalVector, MalHashMap, MalFunction } from "./types";
import { readStr } from "./reader";
import { prStr } from "./printer";

// READ
function read(str: string): MalType {
    return readStr(str);
}

interface MalEnvironment {
    [key: string]: MalFunction;
}

// EVAL
function evalMal(ast: MalType, env: MalEnvironment): MalType {
    // console.log("EVAL:", prStr(ast));
    // Deal with non-list types.
    switch (ast.type) {
        case Node.Symbol:
            const f = env[ast.v];
            if (!f) {
                throw new Error(`'${ast.v}' not found`);
            }
            return f;
        case Node.List:
            break;
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
    if (ast.list.length === 0) {
        return ast;
    }
    const f : MalType = evalMal(ast.list[0], env);
    if (f.type !== Node.Function) {
        throw new Error(`unexpected token: ${f.type}, expected: function`);
    }
    const args : Array<MalType> = ast.list.slice(1).map(x => evalMal(x, env));
    return f.func(...args);
}

// PRINT
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
    return print(evalMal(read(str), replEnv));
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
