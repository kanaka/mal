import { MalType, MalSymbol } from "./types";

export class Env {
    data: Map<MalSymbol, MalType>;

    constructor(public outer?: Env) {
        this.data = new Map();
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
            throw new Error(`${key.v} not found`);
        }

        const v = env.data.get(key);
        if (!v) {
            throw new Error(`${key.v} is not exists`);
        }

        return v;
    }
}
