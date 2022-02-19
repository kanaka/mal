import { MalType, MalNumber, MalList, MalSymbol, MalTypes, MalBoolean, MalString, keywordPrefix, MalMap, MalFunc } from "./types";

export function pr_str(data: MalType, print_readably: boolean): string {
    let str = ""
    switch (data.type) {
        case MalTypes.List:
            str += pr_list(data as MalList)
            break
        case MalTypes.Map:
            str += pr_map(data as MalMap)
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
// TODO: combine pr_map and pr_list into one function
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
            case MalTypes.Map:
                str += pr_map(mal as MalMap)
                break
            case MalTypes.Function:
                str += (mal as MalFunc).f.toString()
        }
        str += " "
    }
    if (str.length === 1) return str + end
    else return str.slice(0, str.length-1) + end
}

function pr_map(map: MalMap): string {
    let str = "{"
    for (const [k, v]  of map.map) {
        str += pr_str(k, true) + " " + pr_str(v, true) + " "
    }
    if (str.length === 1) return str + "}"
    else return str.slice(0, str.length-1) + "}"
} 
