import { Env } from './env'

export type MalType = MalAtom | MalList | MalFunc
export type MalAtom = MalNil | MalNumber | MalSymbol | MalBoolean | MalString

export const keywordPrefix = "\u029E"

export const enum MalTypes {
    List = 1,
    Map,
    Number, 
    String,
    Symbol,
    Nil,
    Boolean,
    Function, 
}

export class MalNumber {
    type = MalTypes.Number
    value: number

    constructor(value: number) {
        this.value = value
    }
}

//TODO: fix value, rawValue representation
export class MalString {
    type = MalTypes.String
    rawValue: string
    value: string

    constructor(rawValue: string) {
        this.rawValue = rawValue
        this.value = rawValue
                        .replace(/\\"/g, '"')
                        .replace(/\\n/g, "\n")

    }
}

export class MalSymbol {
    type = MalTypes.Symbol
    value: string

    constructor(value: string) {
        this.value = value
    }
}

export class MalNil {
    type = MalTypes.Nil
}

export class MalBoolean {
    type = MalTypes.Boolean
    value: Boolean

    constructor(value: Boolean) {
        this.value = value
    }
}

export class MalList {
    type = MalTypes.List
    isVector: boolean
    list: Array<MalType>

    constructor(list: Array<MalType>, isVector: boolean = false) {
        this.list = list
        this.isVector = isVector
    }

    // just for brevity
    push(data: MalType) {
        this.list.push(data)
    }
}

export class MalMap {
    type = MalTypes.Map
    map = new Map<MalString, MalType>()
}

export class MalFunc {
    type = MalTypes.Function
    f: (...args: (MalType)[]) => MalType
    bodyAst?: MalType
    params?: MalSymbol[]
    env?: Env
    
    constructor(f: (...args: (MalType)[]) => MalType, bodyAst?: MalType, params?: MalSymbol[], env?: Env) {
        this.f = f
    }

    static fromLisp(f: (...args: (MalType)[]) => MalType, bodyAst: MalType, params: MalSymbol[], env: Env): MalFunc {
        return new MalFunc(f, bodyAst, params, env)
    }

    apply(args: MalList): MalType {
        return this.f.apply(null, args.list)  
    }
}

export function equals(a: MalType, b: MalType): MalBoolean {
    return new MalBoolean(_equals(a, b))
}

function _equals(a: MalType, b: MalType): boolean {
    if (a.type !== b.type) return false

    switch (a.type, b.type) {
        case MalTypes.Number, MalTypes.Number:
            return (a as MalNumber).value === (b as MalNumber).value
        case MalTypes.String, MalTypes.String:
            return (a as MalString).value === (b as MalString).value
        case MalTypes.Nil, MalTypes.Nil:
            return true
        case MalTypes.Boolean, MalTypes.Boolean:
            return (a as MalBoolean).value === (b as MalBoolean).value
        case MalTypes.Symbol, MalTypes.Symbol:
            return (a as MalSymbol).value === (b as MalSymbol).value
        case MalTypes.List, MalTypes.List:
        
            const aList = (a as MalList)
            const bList = (b as MalList)
            let equal = true
            if (aList.list.length !== bList.list.length) return false
            for (let i = 0; i < aList.list.length; i++) {
                equal = equal && _equals(aList.list[i], bList.list[i])
            }
            return equal
        case MalTypes.Map, MalTypes.Map:
            const aMap = (a as MalMap)
            const bMap = (b as MalMap)
            const aKeys = Array.from(aMap.map.keys())
            const bKeys = Array.from(bMap.map.keys())
            if (aKeys.length !== bKeys.length) return false

            for (const aKey of aKeys) {
                const aVal = aMap.map.get(aKey) ? aMap.map.get(aKey) as MalType : new MalNil()
                const bVal = bMap.map.get(aKey) ? bMap.map.get(aKey) as MalType : new MalNil()
                if (!_equals(aVal, bVal)) return false
            }
            for (const bKey of bKeys) {
                const aVal = aMap.map.get(bKey) ? aMap.map.get(bKey) as MalType : new MalNil()
                const bVal = bMap.map.get(bKey) ? bMap.map.get(bKey) as MalType : new MalNil()
                if (!_equals(aVal, bVal)) return false
            }
            return true
        case MalTypes.Function, MalTypes.Function:
            return (a as MalFunc).f == (b as MalFunc).f
        default:
            return false
    }
    
}