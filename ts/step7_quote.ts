import { readline } from "./node_readline";

import { MalType, MalString, MalBoolean, MalNull, MalList, MalVector, MalHashMap, MalSymbol, MalFunction } from "./types";
import { Env } from "./env";
import * as core from "./core";
import { readStr } from "./reader";
import { prStr } from "./printer";

function read(str: string): MalType {
    return readStr(str);
}

function quasiquote(ast: MalType): MalType {
    if (!isPair(ast)) {
        return new MalList([MalSymbol.get("quote"), ast]);
    }
    if (!MalList.is(ast) && !MalVector.is(ast)) {
        throw new Error(`unexpected token type: ${ast.type}, expected: list or vector`);
    }
    const [arg1, arg2] = ast.list;
    if (MalSymbol.is(arg1) && arg1.v === "unquote") {
        return arg2;
    }
    if (isPair(arg1)) {
        if (!MalList.is(arg1) && !MalVector.is(arg1)) {
            throw new Error(`unexpected token type: ${arg1.type}, expected: list or vector`);
        }
        const [arg11, arg12] = arg1.list;
        if (MalSymbol.is(arg11) && arg11.v === "splice-unquote") {
            return new MalList([
                MalSymbol.get("concat"),
                arg12,
                quasiquote(new MalList(ast.list.slice(1))),
            ]);
        }
    }

    return new MalList([
        MalSymbol.get("cons"),
        quasiquote(arg1),
        quasiquote(new MalList(ast.list.slice(1))),
    ]);

    function isPair(ast: MalType) {
        if (!MalList.is(ast) && !MalVector.is(ast)) {
            return false;
        }

        return 0 < ast.list.length;
    }
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
    loop: while (true) {
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
                        if (!MalSymbol.is(key)) {
                            throw new Error(`unexpected token type: ${key.type}, expected: symbol`);
                        }
                        if (!value) {
                            throw new Error(`unexpected syntax`);
                        }
                        return env.set(key, evalSexp(value, env))
                    }
                    case "let*": {
                        env = new Env(env);
                        const pairs = ast.list[1];
                        if (!MalList.is(pairs) && !MalVector.is(pairs)) {
                            throw new Error(`unexpected token type: ${pairs.type}, expected: list or vector`);
                        }
                        for (let i = 0; i < pairs.list.length; i += 2) {
                            const key = pairs.list[i];
                            const value = pairs.list[i + 1];
                            if (!MalSymbol.is(key)) {
                                throw new Error(`unexpected token type: ${key.type}, expected: symbol`);
                            }
                            if (!key || !value) {
                                throw new Error(`unexpected syntax`);
                            }

                            env.set(key, evalSexp(value, env));
                        }
                        ast = ast.list[2];
                        continue loop;
                    }
                    case "quote": {
                        return ast.list[1];
                    }
                    case "quasiquote": {
                        ast = quasiquote(ast.list[1]);
                        continue loop;
                    }
                    case "do": {
                        const [, ...list] = ast.list;
                        const ret = evalAST(new MalList(list), env);
                        if (!MalList.is(ret) && !MalVector.is(ret)) {
                            throw new Error(`unexpected return type: ${ret.type}, expected: list or vector`);
                        }
                        ast = ret.list[ret.list.length - 1];
                        continue loop;
                    }
                    case "if": {
                        const [, cond, thenExpr, elseExrp] = ast.list;
                        const ret = evalSexp(cond, env);
                        let b = true;
                        if (MalBoolean.is(ret) && !ret.v) {
                            b = false;
                        } else if (MalNull.is(ret)) {
                            b = false;
                        }
                        if (b) {
                            ast = thenExpr;
                        } else if (elseExrp) {
                            ast = elseExrp;
                        } else {
                            ast = MalNull.instance;
                        }
                        continue loop;
                    }
                    case "fn*": {
                        const [, params, bodyAst] = ast.list;
                        if (!MalList.is(params) && !MalVector.is(params)) {
                            throw new Error(`unexpected return type: ${params.type}, expected: list or vector`);
                        }
                        const symbols = params.list.map(param => {
                            if (!MalSymbol.is(param)) {
                                throw new Error(`unexpected return type: ${param.type}, expected: symbol`);
                            }
                            return param;
                        });
                        return MalFunction.fromLisp(evalSexp, env, symbols, bodyAst);
                    }
                }
        }
        const result = evalAST(ast, env);
        if (!MalList.is(result) && !MalVector.is(result)) {
            throw new Error(`unexpected return type: ${result.type}, expected: list or vector`);
        }
        const [f, ...args] = result.list;
        if (!MalFunction.is(f)) {
            throw new Error(`unexpected token: ${f.type}, expected: function`);
        }
        if (f.ast) {
            ast = f.ast;
            env = f.newEnv(args);
            continue loop;
        }

        return f.func(...args);
    }
}

function print(exp: MalType): string {
    return prStr(exp);
}

const replEnv = new Env();
for (const [key, value] of core.ns) {
    replEnv.set(key, value);
}
replEnv.set(MalSymbol.get("eval"), MalFunction.fromBootstrap(ast => {
    if (!ast) {
        throw new Error(`undefined argument`);
    }
    return evalSexp(ast, replEnv);
}));

replEnv.set(MalSymbol.get("*ARGV*"), new MalList([]));

// core.mal: defined using the language itself
rep("(def! not (fn* (a) (if a false true)))");
rep(`(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) ")")))))`);

if (typeof process !== "undefined" && 2 < process.argv.length) {
    replEnv.set(MalSymbol.get("*ARGV*"), new MalList(process.argv.slice(3).map(s => new MalString(s))));
    rep(`(load-file "${process.argv[2]}")`);
    process.exit(0);
}

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
