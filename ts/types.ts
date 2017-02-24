import { Env } from "./env";

export type MalType = MalList | MalNumber | MalString | MalNull | MalBoolean | MalSymbol | MalKeyword | MalVector | MalHashMap | MalFunction;

export function equals(a: MalType, b: MalType, strict?: boolean): boolean {
    if (strict && a.constructor !== b.constructor) {
        return false;
    } else if (
        (MalList.is(a) || MalVector.is(a))
        && (MalList.is(b) || MalVector.is(b))
    ) {
        return listEquals(a.list, b.list);
    }

    if (MalNull.is(a) && MalNull.is(b)) {
        return true;
    }
    if (
        (MalList.is(a) && MalList.is(b))
        || (MalVector.is(a) && MalVector.is(b))
    ) {
        return listEquals(a.list, b.list);
    }
    if (
        (MalNumber.is(a) && MalNumber.is(b))
        || (MalString.is(a) && MalString.is(b))
        || (MalBoolean.is(a) && MalBoolean.is(b))
        || (MalSymbol.is(a) && MalSymbol.is(b))
        || (MalKeyword.is(a) && MalKeyword.is(b))
    ) {
        return a.v === b.v;
    }

    return false;

    function listEquals(a: MalType[], b: MalType[]): boolean {
        if (a.length !== b.length) {
            return false;
        }
        for (let i = 0; i < a.length; i++) {
            if (!equals(a[i], b[i], strict)) {
                return false;
            }
        }
        return true;
    }
}

export class MalList {
    static is(f: MalType): f is MalList {
        return f instanceof MalList;
    }

    type: "list" = "list";

    constructor(public list: MalType[]) {
    }
}

export class MalNumber {
    static is(f: MalType): f is MalNumber {
        return f instanceof MalNumber;
    }

    type: "number" = "number";
    constructor(public v: number) {
    }
}

export class MalString {
    static is(f: MalType): f is MalString {
        return f instanceof MalString;
    }

    type: "string" = "string";
    constructor(public v: string) {
    }
}

export class MalNull {
    static is(f: MalType): f is MalNull {
        return f instanceof MalNull;
    }

    static instance = new MalNull();
    type: "null" = "null";

    private constructor() { }
}

export class MalBoolean {
    static is(f: MalType): f is MalBoolean {
        return f instanceof MalBoolean;
    }

    type: "boolean" = "boolean";
    constructor(public v: boolean) {
    }
}

export class MalSymbol {
    static is(f: MalType): f is MalSymbol {
        return f instanceof MalSymbol;
    }

    static map = new Map<symbol, MalSymbol>();

    static get(name: string): MalSymbol {
        const sym = Symbol.for(name);
        let token = this.map.get(sym);
        if (token) {
            return token;
        }
        token = new MalSymbol(name);
        this.map.set(sym, token);
        return token;
    }

    type: "symbol" = "symbol";

    private constructor(public v: string) {
    }
}

export class MalKeyword {
    static is(f: MalType): f is MalKeyword {
        return f instanceof MalKeyword;
    }

    type: "keyword" = "keyword";
    constructor(public v: string) {
        this.v = String.fromCodePoint(0x29E) + this.v;
    }
}

export class MalVector {
    static is(f: MalType): f is MalVector {
        return f instanceof MalVector;
    }

    type: "vector" = "vector";
    constructor(public list: MalType[]) {
    }
}

export class MalHashMap {
    static is(f: MalType): f is MalHashMap {
        return f instanceof MalHashMap;
    }

    type: "hash-map" = "hash-map";
    map = new Map<MalType, MalType>();
    constructor(list: MalType[]) {
        while (list.length !== 0) {
            const key = list.shift()!;
            const value = list.shift();
            if (value == null) {
                throw new Error("unexpected hash length");
            }
            this.map.set(key, value);
        }
    }
}

type MalF = (...args: (MalType | undefined)[]) => MalType;

export class MalFunction {
    static is(f: MalType): f is MalFunction {
        return f instanceof MalFunction;
    }

    static fromLisp(evalSexpr: (ast: MalType, env: Env) => MalType, env: Env, params: MalSymbol[], bodyAst: MalType): MalFunction {
        const f = new MalFunction();
        f.func = (...args) => evalSexpr(bodyAst, new Env(env, params, malTypes2malSymbols(args)));
        f.env = env;
        f.params = params;
        f.ast = bodyAst;

        return f;

        function malTypes2malSymbols(args: (MalType | undefined)[]): MalSymbol[] {
            return args.map(arg => {
                if (!arg) {
                    throw new Error(`undefined argument`);
                }
                if (!MalSymbol.is(arg)) {
                    throw new Error(`unexpected token type: ${arg.type}, expected: symbol`);
                }
                return arg;
            });
        }
    }

    static fromBootstrap(func: MalF): MalFunction {
        const f = new MalFunction();
        f.func = func;
        return f;
    }

    type: "function" = "function";

    func: MalF;
    ast: MalType;
    env: Env;
    params: MalSymbol[];

    private constructor() { }

    newEnv(args: MalType[]) {
        return new Env(this.env, this.params, args);
    }
}
