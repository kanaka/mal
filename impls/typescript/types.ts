export type MalType = MalAtom | MalList
// export type MalAtom = number  string // TODO: Add nil, true, false and string
// export type MalList = Array<MalAtom | MalList>


function isAtom(data: any): data is MalAtom {
    return (data as MalAtom).valueOf !== undefined
}



export class MalAtom {
    type = "atom"
    value: number | string

    constructor(value: number | string) {
        this.value = value
    }
}

export class MalList {
    type = "list"
    list: Array<MalType>

    constructor(list: Array<MalAtom | MalList>) {
        this.list = list
    }

    // just for brevity
    push(data: MalType) {
        this.list.push(data)
    }
}

