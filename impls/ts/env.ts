import { MalType, MalSymbol, MalList } from "./types";

export class Env {
    data: Map<string, MalType>;

    constructor(public outer?: Env, binds: MalSymbol[] = [], exprts: MalType[] = []) {
        this.data = new Map();

        for (let i = 0; i < binds.length; i++) {
            const bind : string = binds[i].v;
            if (bind === "&") {
                this.set(binds[i + 1].v, new MalList(exprts.slice(i)));
                break;
            }
            this.set(bind, exprts[i]);
        }
    }

    set(key: string, value: MalType): MalType {
        this.data.set(key, value);
        return value;
    }

    get(key: string): MalType | null {
        const result : MalType | undefined = this.data.get(key);
        if (result) {
            return result;
        } else if (this.outer) {
            return this.outer.get(key);
        } else {
            return null;
        }
    }
}
