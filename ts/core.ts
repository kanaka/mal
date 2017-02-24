import * as fs from "fs";

import { MalType, MalSymbol, MalFunction, MalNull, MalList, MalVector, MalBoolean, MalNumber, MalString, MalAtom, equals } from "./types";
import { readStr } from "./reader";
import { prStr } from "./printer";

export const ns: Map<MalSymbol, MalFunction> = (() => {
    const ns: { [symbol: string]: typeof MalFunction.prototype.func; } = {
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
    };

    const map = new Map<MalSymbol, MalFunction>();
    Object.keys(ns).forEach(key => map.set(MalSymbol.get(key), MalFunction.fromBootstrap(ns[key])));
    return map;
})();
