import { MalType, MalNumber, MalList, MalSymbol, MalTypes, MalBoolean, MalString, keywordPrefix } from "./types";

export function pr_str(data: MalType, print_readably: boolean): string {
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
        case MalTypes.String:
            // handle keywords
            const malStr = (data as MalString)
            if (malStr.rawValue.startsWith(keywordPrefix)) {
                str += ":" + malStr.rawValue.slice(1)
            } else {
                str += print_readably 
                    ? '"' + malStr.rawValue + '"'
                    : malStr.value
            }
    }    
    return str 
}
function pr_list(list: MalList): string {
    const start = list.isVector ? "[" : "("
    const end = list.isVector ? "]" : ")"
    let str = start
    const arr = list.list
    for (const mal of arr) {
        switch (mal.type) {
            case MalTypes.Number:
            case MalTypes.Symbol:
                str += pr_str(mal, true)
                break
            case MalTypes.List:
                str += pr_list(mal as MalList)
                break
        }
        str += " "
    }
    if (str.length === 1) return str + end
    else return str.slice(0, str.length-1) + end
}
