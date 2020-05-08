import { MalType, MalSymbol, MalList } from "./types";

export class Env {
    data: Map<MalSymbol, MalType>;

    constructor(public outer?: Env, binds: MalSymbol[] = [], exprts: MalType[] = []) {
        this.data = new Map();

        for (let i = 0; i < binds.length; i++) {
            const bind = binds[i];
            if (bind.v === "&") {
                this.set(binds[i + 1], new MalList(exprts.slice(i)));
                break;
            }
            this.set(bind, exprts[i]);
        }
    }

    set(key: MalSymbol, value: MalType): MalType {
        this.data.set(key, value);
        return value;
    }

    find(key: MalSymbol): Env | undefined {
        if (this.data.has(key)) {
            return this;
        }
        if (this.outer) {
            return this.outer.find(key);
        }

        return void 0;
    }

    get(key: MalSymbol): MalType {
        const env = this.find(key);
        if (!env) {
            throw new Error(`'${key.v}' not found`);
        }

        const v = env.data.get(key);
        if (!v) {
            throw new Error(`'${key.v}' not found`);
        }

        return v;
    }
}
