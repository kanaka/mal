import { pr_str } from './printer'
import { MalSymbol, MalFunc, MalType, MalBoolean, MalNumber, MalNil, MalList, MalTypes, equals, MalString,} from './types'

export const ns: Map<string, MalFunc> = (() => {
    const ns: { [symbol: string]: typeof MalFunc.prototype.f; } = {
        "+": (a: MalNumber, b: MalNumber) => new MalNumber(a.value + b.value),
        "-": (a: MalNumber, b: MalNumber) => new MalNumber(a.value - b.value),
        "*": (a: MalNumber, b: MalNumber) => new MalNumber(a.value * b.value),
        "/": (a: MalNumber, b: MalNumber) => new MalNumber(a.value / b.value),
        "list": (...params: MalType[]) => new MalList(params, false),
        "list?": (a: MalType) => new MalBoolean(a.type === MalTypes.List && !(a as MalList).isVector),
        "empty?": (a: MalType) => new MalBoolean((a as MalList).list.length === 0),
        "count": (a: MalType) => (a.type === MalTypes.Nil) ? new MalNumber(0) : new MalNumber((a as MalList).list.length),
        "=": (a: MalType, b: MalType) => equals(a, b),
        "<": (a: MalNumber, b: MalNumber) => new MalBoolean(a.value < b.value),
        "<=": (a: MalNumber, b: MalNumber) => new MalBoolean(a.value <= b.value),
        ">": (a: MalNumber, b: MalNumber) => new MalBoolean(a.value > b.value),
        ">=": (a: MalNumber, b: MalNumber) => new MalBoolean(a.value >= b.value),
        "pr-str": (...params: MalType[]) => new MalString(params.map(s => pr_str(s, true)).join(" ")),
        "str": (...params: MalType[]) => new MalString(params.map(s => pr_str(s, false)).join(" ")),
        "prn": (...params: MalType[]) => {
            console.log(params.map(s => pr_str(s, true)).join(" "))
            return new MalNil()
        },
        "println": (...params: MalType[]) => {
            console.log(params.map(s => pr_str(s, false)).join(" "))
            return new MalNil()
        }
    }
    const map: Map<string, MalFunc> = new Map()
    Object.keys(ns).forEach(key => map.set(key, new MalFunc(ns[key])))

    return map
})()


export const not = "(def! not (fn* (a) (if a false true)))"
