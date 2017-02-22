export type MalType = MalList | MalNumber | MalString | MalNull | MalBoolean | MalSymbol | MalKeyword | MalVector | MalHashMap;

export class MalList {
    type: "list" = "list";

    constructor(public list: MalType[]) {
    }
}

export class MalNumber {
    type: "number" = "number";
    constructor(public v: number) {
    }
}

export class MalString {
    type: "string" = "string";
    constructor(public v: string) {
    }
}

export class MalNull {
    type: "null" = "null";
}

export class MalBoolean {
    type: "boolean" = "boolean";
    constructor(public v: boolean) {
    }
}

export class MalSymbol {
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
    type: "keyword" = "keyword";
    constructor(public v: string) {
        this.v = String.fromCodePoint(0x29E) + this.v;
    }
}

export class MalVector {
    type: "vector" = "vector";
    constructor(public list: MalType[]) {
    }
}

export class MalHashMap {
    type: "hash-map" = "hash-map";
    map = new Map<MalType, MalType>();
    constructor(list: MalType[]) {
        while (list.length !== 0) {
            const key = list.shift() !;
            const value = list.shift();
            if (value == null) {
                throw new Error("unexpected hash length");
            }
            this.map.set(key, value);
        }
    }
}
