import * as fs from "fs";

import { readline } from "./node_readline";

import { MalType, MalSymbol, MalFunction, MalNull, MalList, MalVector, MalBoolean, MalNumber, MalString, MalKeyword, MalHashMap, MalAtom, equals } from "./types";
import { readStr } from "./reader";
import { prStr } from "./printer";

export const ns: Map<MalSymbol, MalFunction> = (() => {
    const ns: { [symbol: string]: typeof MalFunction.prototype.func; } = {
        readline(v: MalType) {
            if (!MalString.is(v)) {
                throw new Error(`unexpected symbol: ${v.type}, expected: string`);
            }

            const ret = readline(v.v);
            if (ret == null) {
                return MalNull.instance;
            }

            return new MalString(ret);
        },
        "pr-str"(...args: MalType[]): MalString {
            return new MalString(args.map(v => prStr(v, true)).join(" "));
        },
        "str"(...args: MalType[]): MalString {
            return new MalString(args.map(v => prStr(v, false)).join(""));
        },
        prn(...args: MalType[]): MalNull {
            const str = args.map(v => prStr(v, true)).join(" ");
            console.log(str);
            return MalNull.instance;
        },
        println(...args: MalType[]): MalNull {
            const str = args.map(v => prStr(v, false)).join(" ");
            console.log(str);
            return MalNull.instance;
        },
        "read-string"(v: MalType) {
            if (!MalString.is(v)) {
                throw new Error(`unexpected symbol: ${v.type}, expected: string`);
            }
            return readStr(v.v);
        },
        slurp(v: MalType) {
            if (!MalString.is(v)) {
                throw new Error(`unexpected symbol: ${v.type}, expected: string`);
            }
            const content = fs.readFileSync(v.v, "UTF-8");
            return new MalString(content);
        },
        cons(a: MalType, b: MalType) {
            if (!MalList.is(b) && !MalVector.is(b)) {
                throw new Error(`unexpected symbol: ${b.type}, expected: list or vector`);
            }

            return new MalList([a].concat(b.list));
        },
        concat(...args: MalType[]) {
            const list = args
                .map(arg => {
                    if (!MalList.is(arg) && !MalVector.is(arg)) {
                        throw new Error(`unexpected symbol: ${arg.type}, expected: list or vector`);
                    }
                    return arg;
                })
                .reduce((p, c) => p.concat(c.list), [] as MalType[]);

            return new MalList(list);
        },
        list(...args: MalType[]): MalList {
            return new MalList(args);
        },
        "list?"(v: MalType): MalBoolean {
            return new MalBoolean(v instanceof MalList);
        },
        "empty?"(v: MalType): MalBoolean {
            if (!MalList.is(v) && !MalVector.is(v)) {
                return new MalBoolean(false);
            }
            return new MalBoolean(v.list.length === 0);
        },
        count(v: MalType): MalNumber {
            if (MalList.is(v) || MalVector.is(v)) {
                return new MalNumber(v.list.length);
            }
            if (MalNull.is(v)) {
                return new MalNumber(0);
            }
            throw new Error(`unexpected symbol: ${v.type}`);
        },
        nth(list: MalType, idx: MalType) {
            if (!MalList.is(list) && !MalVector.is(list)) {
                throw new Error(`unexpected symbol: ${list.type}, expected: list or vector`);
            }
            if (!MalNumber.is(idx)) {
                throw new Error(`unexpected symbol: ${idx.type}, expected: number`);
            }

            const v = list.list[idx.v];
            if (!v) {
                throw new Error("nth: index out of range");
            }

            return v;
        },
        first(v: MalType) {
            if (MalNull.is(v)) {
                return MalNull.instance;
            }
            if (!MalList.is(v) && !MalVector.is(v)) {
                throw new Error(`unexpected symbol: ${v.type}, expected: list or vector`);
            }

            return v.list[0] || MalNull.instance;
        },
        rest(v: MalType) {
            if (MalNull.is(v)) {
                return new MalList([]);
            }
            if (!MalList.is(v) && !MalVector.is(v)) {
                throw new Error(`unexpected symbol: ${v.type}, expected: list or vector`);
            }

            return new MalList(v.list.slice(1));
        },
        atom(v: MalType): MalAtom {
            return new MalAtom(v);
        },
        "atom?"(v: MalType): MalBoolean {
            return new MalBoolean(MalAtom.is(v));
        },
        deref(v: MalType): MalType {
            if (!MalAtom.is(v)) {
                throw new Error(`unexpected symbol: ${v.type}, expected: atom`);
            }
            return v.v;
        },
        "reset!"(atom: MalType, v: MalType): MalType {
            if (!MalAtom.is(atom)) {
                throw new Error(`unexpected symbol: ${atom.type}, expected: atom`);
            }
            atom.v = v;
            return v;
        },
        "swap!"(atom: MalType, f: MalType, ...args: MalType[]): MalType {
            if (!MalAtom.is(atom)) {
                throw new Error(`unexpected symbol: ${atom.type}, expected: atom`);
            }
            if (!MalFunction.is(f)) {
                throw new Error(`unexpected symbol: ${f.type}, expected: function`);
            }
            atom.v = f.func(...[atom.v].concat(args));
            return atom.v;
        },
        throw(v: MalType): MalType {
            throw v;
        },
        apply(f: MalType, ...list: MalType[]) {
            if (!MalFunction.is(f)) {
                throw new Error(`unexpected symbol: ${f.type}, expected: function`);
            }

            const tail = list[list.length - 1];
            if (!MalList.is(tail) && !MalVector.is(tail)) {
                throw new Error(`unexpected symbol: ${tail.type}, expected: list or vector`);
            }
            const args = list.slice(0, -1).concat(tail.list);
            return f.func(...args);
        },
        map(f: MalType, list: MalType) {
            if (!MalFunction.is(f)) {
                throw new Error(`unexpected symbol: ${f.type}, expected: function`);
            }
            if (!MalList.is(list) && !MalVector.is(list)) {
                throw new Error(`unexpected symbol: ${list.type}, expected: list or vector`);
            }

            return new MalList(list.list.map(v => f.func(v)));
        },
        "+"(a: MalType, b: MalType): MalNumber {
            if (!MalNumber.is(a)) {
                throw new Error(`unexpected symbol: ${a.type}, expected: number`);
            }
            if (!MalNumber.is(b)) {
                throw new Error(`unexpected symbol: ${b.type}, expected: number`);
            }

            return new MalNumber(a.v + b.v);
        },
        "-"(a: MalType, b: MalType): MalNumber {
            if (!MalNumber.is(a)) {
                throw new Error(`unexpected symbol: ${a.type}, expected: number`);
            }
            if (!MalNumber.is(b)) {
                throw new Error(`unexpected symbol: ${b.type}, expected: number`);
            }

            return new MalNumber(a.v - b.v);
        },
        "*"(a: MalType, b: MalType): MalNumber {
            if (!MalNumber.is(a)) {
                throw new Error(`unexpected symbol: ${a.type}, expected: number`);
            }
            if (!MalNumber.is(b)) {
                throw new Error(`unexpected symbol: ${b.type}, expected: number`);
            }

            return new MalNumber(a.v * b.v);
        },
        "/"(a: MalType, b: MalType): MalNumber {
            if (!MalNumber.is(a)) {
                throw new Error(`unexpected symbol: ${a.type}, expected: number`);
            }
            if (!MalNumber.is(b)) {
                throw new Error(`unexpected symbol: ${b.type}, expected: number`);
            }

            return new MalNumber(a.v / b.v);
        },
        "="(a: MalType, b: MalType): MalBoolean {
            return new MalBoolean(equals(a, b));
        },
        "<"(a: MalType, b: MalType): MalBoolean {
            if (!MalNumber.is(a)) {
                throw new Error(`unexpected symbol: ${a.type}, expected: number`);
            }
            if (!MalNumber.is(b)) {
                throw new Error(`unexpected symbol: ${b.type}, expected: number`);
            }

            return new MalBoolean(a.v < b.v);
        },
        "<="(a: MalType, b: MalType): MalBoolean {
            if (!MalNumber.is(a)) {
                throw new Error(`unexpected symbol: ${a.type}, expected: number`);
            }
            if (!MalNumber.is(b)) {
                throw new Error(`unexpected symbol: ${b.type}, expected: number`);
            }

            return new MalBoolean(a.v <= b.v);
        },
        ">"(a: MalType, b: MalType): MalBoolean {
            if (!MalNumber.is(a)) {
                throw new Error(`unexpected symbol: ${a.type}, expected: number`);
            }
            if (!MalNumber.is(b)) {
                throw new Error(`unexpected symbol: ${b.type}, expected: number`);
            }

            return new MalBoolean(a.v > b.v);
        },
        ">="(a: MalType, b: MalType): MalBoolean {
            if (!MalNumber.is(a)) {
                throw new Error(`unexpected symbol: ${a.type}, expected: number`);
            }
            if (!MalNumber.is(b)) {
                throw new Error(`unexpected symbol: ${b.type}, expected: number`);
            }

            return new MalBoolean(a.v >= b.v);
        },
        "time-ms"() {
            return new MalNumber(Date.now());
        },
        "nil?"(v: MalType) {
            return new MalBoolean(MalNull.is(v));
        },
        "true?"(v: MalType) {
            return new MalBoolean(MalBoolean.is(v) && v.v);
        },
        "false?"(v: MalType) {
            return new MalBoolean(MalBoolean.is(v) && !v.v);
        },
        "string?"(v: MalType) {
            return new MalBoolean(MalString.is(v));
        },
        "symbol?"(v: MalType) {
            return new MalBoolean(MalSymbol.is(v));
        },
        symbol(v: MalType) {
            if (!MalString.is(v)) {
                throw new Error(`unexpected symbol: ${v.type}, expected: string`);
            }
            return MalSymbol.get(v.v);
        },
        keyword(v: MalType) {
            if (!MalString.is(v)) {
                throw new Error(`unexpected symbol: ${v.type}, expected: string`);
            }
            return MalKeyword.get(v.v);
        },
        "keyword?"(v: MalType) {
            return new MalBoolean(MalKeyword.is(v));
        },
        vector(...args: MalType[]): MalVector {
            return new MalVector(args);
        },
        "vector?"(v: MalType): MalBoolean {
            return new MalBoolean(MalVector.is(v));
        },
        "hash-map"(...args: MalType[]) {
            return new MalHashMap(args);
        },
        "map?"(v: MalType): MalBoolean {
            return new MalBoolean(MalHashMap.is(v));
        },
        assoc(v: MalType, ...args: MalType[]) {
            if (!MalHashMap.is(v)) {
                throw new Error(`unexpected symbol: ${v.type}, expected: hash-map`);
            }
            return v.assoc(args);
        },
        dissoc(v: MalType, ...args: MalType[]) {
            if (!MalHashMap.is(v)) {
                throw new Error(`unexpected symbol: ${v.type}, expected: hash-map`);
            }
            return v.dissoc(args);
        },
        get(v: MalType, key: MalType) {
            if (MalNull.is(v)) {
                return MalNull.instance;
            }
            if (!MalHashMap.is(v)) {
                throw new Error(`unexpected symbol: ${v.type}, expected: hash-map`);
            }
            if (!MalString.is(key) && !MalKeyword.is(key)) {
                throw new Error(`unexpected symbol: ${key.type}, expected: string or keyword`);
            }

            return v.get(key) || MalNull.instance;
        },
        "contains?"(v: MalType, key: MalType) {
            if (MalNull.is(v)) {
                return MalNull.instance;
            }
            if (!MalHashMap.is(v)) {
                throw new Error(`unexpected symbol: ${v.type}, expected: hash-map`);
            }
            if (!MalString.is(key) && !MalKeyword.is(key)) {
                throw new Error(`unexpected symbol: ${key.type}, expected: string or keyword`);
            }

            return new MalBoolean(v.has(key));
        },
        keys(v: MalType) {
            if (!MalHashMap.is(v)) {
                throw new Error(`unexpected symbol: ${v.type}, expected: hash-map`);
            }

            return new MalList([...v.keys()]);
        },
        vals(v: MalType) {
            if (!MalHashMap.is(v)) {
                throw new Error(`unexpected symbol: ${v.type}, expected: hash-map`);
            }

            return new MalList([...v.vals()]);
        },
        "sequential?"(v: MalType) {
            return new MalBoolean(MalList.is(v) || MalVector.is(v));
        },
        conj(list: MalType, ...args: MalType[]) {
            switch (list.type) {
                case "list":
                    const newList = new MalList(list.list);
                    args.forEach(arg => newList.list.unshift(arg));
                    return newList;
                case "vector":
                    return new MalVector([...list.list, ...args]);
            }

            throw new Error(`unexpected symbol: ${list.type}, expected: list or vector`);
        },
        seq(v: MalType) {
            if (MalList.is(v)) {
                if (v.list.length === 0) {
                    return MalNull.instance;
                }
                return v;
            }
            if (MalVector.is(v)) {
                if (v.list.length === 0) {
                    return MalNull.instance;
                }
                return new MalList(v.list);
            }
            if (MalString.is(v)) {
                if (v.v.length === 0) {
                    return MalNull.instance;
                }
                return new MalList(v.v.split("").map(s => new MalString(s)));
            }
            if (MalNull.is(v)) {
                return MalNull.instance;
            }

            throw new Error(`unexpected symbol: ${v.type}, expected: list or vector or string`);
        },
        "with-meta"(v: MalType, m: MalType) {
            return v.withMeta(m);
        },
        meta(v: MalType) {
            return v.meta || MalNull.instance;
        },
    };

    const map = new Map<MalSymbol, MalFunction>();
    Object.keys(ns).forEach(key => map.set(MalSymbol.get(key), MalFunction.fromBootstrap(ns[key])));
    return map;
})();
