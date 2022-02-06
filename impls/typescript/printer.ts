import { MalType, MalAtom, MalList } from "./types";

export function pr_str(data: MalType): string {
    let str = ""
    switch (data.type) {
        case "list":
            str += pr_list(data as MalList)
            break
        default:
            str += (data as MalAtom).value.toString()
            break
    }    
    return str 
}


function pr_list(list: MalList): string {
    let str = "("
    const arr = list.list
    for (const mal of arr) {
        switch (mal.type) {
            case "atom":
                str += pr_str(mal)
                break
            case "list":
                str += pr_list(mal as MalList)
                break
        }
        str += " "
    }
    return str.slice(0, str.length-1) + ")"
}
