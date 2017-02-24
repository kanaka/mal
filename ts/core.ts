import { MalType, MalSymbol, MalFunction, MalNull, MalList, MalVector, MalBoolean, MalNumber, MalString, equals } from "./types";
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
