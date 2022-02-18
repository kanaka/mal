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
    
    constructor(f: (...args: (MalType | undefined)[]) => MalType) {
        this.f = f
    }

    apply(args: MalList): MalType {
        return this.f.apply(null, args.list)  
    }
}
