import { Env } from "./env";

export type MalType = MalList | MalNumber | MalString | MalNull | MalBoolean | MalSymbol | MalKeyword | MalVector | MalHashMap | MalFunction | MalAtom;

export function equals(a: MalType, b: MalType, strict?: boolean): boolean {
    if (strict && a.type !== b.type) {
        return false;
    }

    if (MalNull.is(a) && MalNull.is(b)) {
        return true;
    }
    if (isSeq(a) && isSeq(b)) {
        return listEquals(a.list, b.list);
    }
    if (MalHashMap.is(a) && MalHashMap.is(b)) {
        if (a.keywordMap.size !== b.keywordMap.size) {
            return false;
        }
        if (Object.keys(a.stringMap).length !== Object.keys(b.stringMap).length) {
            return false;
        }
        for (const [aK, aV] of a.entries()) {
            if (!MalString.is(aK) && !MalKeyword.is(aK)) {
                throw new Error(`unexpected symbol: ${aK.type}, expected: string or keyword`);
            }
            const bV = b.get(aK);
            if (MalNull.is(aV) && MalNull.is(bV)) {
                continue;
            }
            if (!equals(aV, bV)) {
                return false;
            }
        }

        return true;
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

export function isSeq(ast: MalType): ast is MalList | MalVector {
    return MalList.is(ast) || MalVector.is(ast);
}

export function isAST(v: MalType): v is MalType {
    return !!v.type;
}

export class MalList {
    static is(f: MalType): f is MalList {
        return f instanceof MalList;
    }

    type: "list" = "list";
    meta?: MalType;

    constructor(public list: MalType[]) {
    }

    withMeta(meta: MalType) {
        const v = new MalList(this.list);
        v.meta = meta;
        return v;
    }
}

export class MalNumber {
    static is(f: MalType): f is MalNumber {
        return f instanceof MalNumber;
    }

    type: "number" = "number";
    meta?: MalType;

    constructor(public v: number) {
    }

    withMeta(meta: MalType) {
        const v = new MalNumber(this.v);
        v.meta = meta;
        return v;
    }
}

export class MalString {
    static is(f: MalType): f is MalString {
        return f instanceof MalString;
    }

    type: "string" = "string";
    meta?: MalType;

    constructor(public v: string) {
    }

    withMeta(meta: MalType) {
        const v = new MalString(this.v);
        v.meta = meta;
        return v;
    }
}

export class MalNull {
    static is(f: MalType): f is MalNull {
        return f instanceof MalNull;
    }

    static instance = new MalNull();

    type: "null" = "null";
    meta?: MalType;

    private constructor() { }

    withMeta(_meta: MalType): MalNull {
        throw new Error(`not supported`);
    }
}

export class MalBoolean {
    static is(f: MalType): f is MalBoolean {
        return f instanceof MalBoolean;
    }

    type: "boolean" = "boolean";
    meta?: MalType;

    constructor(public v: boolean) {
    }

    withMeta(meta: MalType) {
        const v = new MalBoolean(this.v);
        v.meta = meta;
        return v;
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
    meta?: MalType;

    private constructor(public v: string) {
    }

    withMeta(_meta: MalType): MalSymbol {
        throw new Error(`not supported`);
    }
}

export class MalKeyword {
    static is(f: MalType): f is MalKeyword {
        return f instanceof MalKeyword;
    }

    static map = new Map<symbol, MalKeyword>();

    static get(name: string): MalKeyword {
        const sym = Symbol.for(name);
        let token = this.map.get(sym);
        if (token) {
            return token;
        }
        token = new MalKeyword(name);
        this.map.set(sym, token);
        return token;
    }

    type: "keyword" = "keyword";
    meta?: MalType;

    private constructor(public v: string) {
    }

    withMeta(_meta: MalType): MalKeyword {
        throw new Error(`not supported`);
    }
}

export class MalVector {
    static is(f: MalType): f is MalVector {
        return f instanceof MalVector;
    }

    type: "vector" = "vector";
    meta?: MalType;

    constructor(public list: MalType[]) {
    }

    withMeta(meta: MalType) {
        const v = new MalVector(this.list);
        v.meta = meta;
        return v;
    }
}

export class MalHashMap {
    static is(f: MalType): f is MalHashMap {
        return f instanceof MalHashMap;
    }

    type: "hash-map" = "hash-map";
    stringMap: { [key: string]: MalType } = {};
    keywordMap = new Map<MalType, MalType>();
    meta?: MalType;

    constructor(list: MalType[]) {
        while (list.length !== 0) {
            const key = list.shift()!;
            const value = list.shift();
            if (value == null) {
                throw new Error("unexpected hash length");
            }
            if (MalKeyword.is(key)) {
                this.keywordMap.set(key, value);
            } else if (MalString.is(key)) {
                this.stringMap[key.v] = value;
            } else {
                throw new Error(`unexpected key symbol: ${key.type}, expected: keyword or string`);
            }
        }
    }

    withMeta(meta: MalType) {
        const v = this.assoc([]);
        v.meta = meta;
        return v;
    }

    has(key: MalKeyword | MalString) {
        if (MalKeyword.is(key)) {
            return !!this.keywordMap.get(key);
        }
        return !!this.stringMap[key.v];
    }

    get(key: MalKeyword | MalString) {
        if (MalKeyword.is(key)) {
            return this.keywordMap.get(key) || MalNull.instance;
        }
        return this.stringMap[key.v] || MalNull.instance;
    }

    entries(): [MalType, MalType][] {
        const list: [MalType, MalType][] = [];

        this.keywordMap.forEach((v, k) => {
            list.push([k, v]);
        });
        Object.keys(this.stringMap).forEach(v => list.push([new MalString(v), this.stringMap[v]]));

        return list;
    }

    keys(): MalType[] {
        const list: MalType[] = [];
        this.keywordMap.forEach((_v, k) => {
            list.push(k);
        });
        Object.keys(this.stringMap).forEach(v => list.push(new MalString(v)));
        return list;
    }

    vals(): MalType[] {
        const list: MalType[] = [];
        this.keywordMap.forEach(v => {
            list.push(v);
        });
        Object.keys(this.stringMap).forEach(v => list.push(this.stringMap[v]));
        return list;
    }

    assoc(args: MalType[]): MalHashMap {
        const list: MalType[] = [];
        this.keywordMap.forEach((value, key) => {
            list.push(key);
            list.push(value);
        });
        Object.keys(this.stringMap).forEach(keyStr => {
            list.push(new MalString(keyStr));
            list.push(this.stringMap[keyStr]);
        });

        return new MalHashMap(list.concat(args));
    }

    dissoc(args: MalType[]): MalHashMap {
        const newHashMap = this.assoc([]);

        args.forEach(arg => {
            if (MalString.is(arg)) {
                delete newHashMap.stringMap[arg.v];
            } else if (MalKeyword.is(arg)) {
                newHashMap.keywordMap.delete(arg);
            } else {
                throw new Error(`unexpected symbol: ${arg.type}, expected: keyword or string`);
            }
        });
        return newHashMap;
    }
}

type MalF = (...args: (MalType | undefined)[]) => MalType;

export class MalFunction {
    static is(f: MalType): f is MalFunction {
        return f instanceof MalFunction;
    }

    static fromLisp(evalMal: (ast: MalType, env: Env) => MalType, env: Env, params: MalSymbol[], bodyAst: MalType): MalFunction {
        const f = new MalFunction();
        f.func = (...args) => evalMal(bodyAst, new Env(env, params, checkUndefined(args)));
        f.env = env;
        f.params = params;
        f.ast = bodyAst;
        f.isMacro = false;

        return f;

        function checkUndefined(args: (MalType | undefined)[]): MalType[] {
            return args.map(arg => {
                if (!arg) {
                    throw new Error(`undefined argument`);
                }
                return arg;
            });
        }
    }

    static fromBootstrap(func: MalF): MalFunction {
        const f = new MalFunction();
        f.func = func;
        f.isMacro = false;

        return f;
    }

    type: "function" = "function";

    func: MalF;
    ast: MalType;
    env: Env;
    params: MalSymbol[];
    isMacro: boolean;
    meta?: MalType;

    private constructor() { }

    withMeta(meta: MalType) {
        const f = new MalFunction();
        f.func = this.func;
        f.ast = this.ast;
        f.env = this.env;
        f.params = this.params;
        f.isMacro = this.isMacro;
        f.meta = meta;

        return f;
    }

    newEnv(args: MalType[]) {
        return new Env(this.env, this.params, args);
    }
}

export class MalAtom {
    static is(f: MalType): f is MalAtom {
        return f instanceof MalAtom;
    }

    type: "atom" = "atom";
    meta?: MalType;

    constructor(public v: MalType) {
    }

    withMeta(meta: MalType) {
        const v = new MalAtom(this.v);
        v.meta = meta;
        return v;
    }
}