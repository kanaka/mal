import { readline } from "./node_readline";

import { Node, MalType, MalNumber, MalVector, MalHashMap, MalFunction, isSeq } from "./types";
import { Env } from "./env";
import { readStr } from "./reader";
import { prStr } from "./printer";

// READ
function read(str: string): MalType {
    return readStr(str);
}

// EVAL
function evalMal(ast: MalType, env: Env): MalType {
    // Output a debug line if the option is enabled.
    const dbgeval : MalType | null = env.get("DEBUG-EVAL");
    if (dbgeval !== null
        && dbgeval.type !== Node.Nil
        && (dbgeval.type !== Node.Boolean || dbgeval.v))
      console.log("EVAL:", prStr(ast));
    // Deal with non-list types.
    switch (ast.type) {
        case Node.Symbol:
            const f : MalType | null = env.get(ast.v);
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
                    return env.set(key.v, evalMal(value, env));
                }
                case "let*": {
                    let letEnv = new Env(env);
                    const pairs = ast.list[1];
                    if (!isSeq(pairs)) {
                        throw new Error(`unexpected token type: ${pairs.type}, expected: list or vector`);
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

                        letEnv.set(key.v, evalMal(value, letEnv));
                    }
                    return evalMal(ast.list[2], letEnv);
                }
            }
    }
    const f : MalType = evalMal(first, env);
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

const replEnv = new Env();
function rep(str: string): string {
    return print(evalMal(read(str), replEnv));
}

replEnv.set("+", MalFunction.fromBootstrap((a?: MalNumber, b?: MalNumber) => new MalNumber(a!.v + b!.v)));
replEnv.set("-", MalFunction.fromBootstrap((a?: MalNumber, b?: MalNumber) => new MalNumber(a!.v - b!.v)));
replEnv.set("*", MalFunction.fromBootstrap((a?: MalNumber, b?: MalNumber) => new MalNumber(a!.v * b!.v)));
replEnv.set("/", MalFunction.fromBootstrap((a?: MalNumber, b?: MalNumber) => new MalNumber(a!.v / b!.v)));

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
