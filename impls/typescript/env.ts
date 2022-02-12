import { MalNumber, MalSymbol, MalType } from "./types"

export class Env {
    outer: Env | undefined
    // TODO: find a way to make key a MalSymbol
    data: {[key: string]: MalType} = {}

    constructor(outer?: Env) {
        if (outer !== undefined) {
            this.outer = outer
        }
    }

    set(key: MalSymbol, value: MalType): void {
        this.data[key.value] = value
    }

    find(key: MalSymbol): Env | undefined {
        if (key.value in this.data){
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
        return env.data[key.value]
    }
}