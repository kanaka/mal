import { MalType, MalNumber, MalList, MalSymbol, MalTypes, MalBoolean } from "./types";

export function pr_str(data: MalType): string {
    let str = ""
    switch (data.type) {
        case MalTypes.List:
            str += pr_list(data as MalList)
            break
        case MalTypes.Number:
            str += (data as MalNumber).value.toString()
            break
        case MalTypes.Symbol:
            str += (data as MalSymbol).value.toString()
            break
        case MalTypes.Nil:
            str += "nil"
            break
        case MalTypes.Boolean:
            str += (data as MalBoolean).value.toString()
            break
    }    
    return str 
}


function pr_list(list: MalList): string {
    let str = "("
    const arr = list.list
    for (const mal of arr) {
        switch (mal.type) {
            case MalTypes.Number:
            case MalTypes.Symbol:
                str += pr_str(mal)
                break
            case MalTypes.List:
                str += pr_list(mal as MalList)
                break
        }
        str += " "
    }
    return str.slice(0, str.length-1) + ")"
}
