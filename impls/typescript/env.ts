import { MalList, MalNumber, MalSymbol, MalType } from "./types"

export class Env {
    outer: Env | undefined
    data: Map<string, MalType> = new Map()

    constructor(outer?: Env, binds: MalSymbol[] = [], exprs: MalType[] = []) {
        this.outer = outer
        
        for (let i = 0; i < binds.length; i++) {
            const bind = binds[i]
            if (bind.value === "&") {
                // Clojure-style variadic function parameters
                this.set(binds[i+1], new MalList(exprs.slice(i)))
                break
            }    
            this.set(bind, exprs[i])
        }
    }

    set(key: MalSymbol, value: MalType): void {
        this.data.set(key.value, value)
    }

    find(key: MalSymbol): Env | undefined {
        if (this.data.has(key.value)){
            return this
        } else if (this.outer === undefined) {
            return undefined
        } else {
            return (this.outer as Env).find(key)
        }
    }

    get(key: MalSymbol): MalType {
        const env = this.find(key)
        if (env === undefined) throw new Error("Symbol \"" + key.value + "\" not recognized")
        return env.data.get(key.value) as MalType
    }
}