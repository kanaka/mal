import { MalType, MalList, MalNumber, MalSymbol, MalNil, MalAtom } from "./types";


class Reader {
    private tokens: string[]
    private currIdx: number = 0

    constructor(tokens: string[]) {
        this.tokens = tokens
    }

    next(): string | null {
        if (this.currIdx >= this.tokens.length) {
            return null
        }
        this.currIdx += 1
        return this.tokens[this.currIdx-1]
    }

    peek(): string | null {
        if (this.currIdx >= this.tokens.length) {
            return null
        }
        return this.tokens[this.currIdx]
    }

}

// TODO: test all scenarios, including special symbols and extra spaces
function tokenize(str: string): string[]{
    const regex = /[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)/g
    const results = []
    let match = regex.exec(str)
    if (match === null) {return [] }
    while (match[1] != '') {
        if (match[0] === ";") { continue }
        results.push(match[1])
        match = regex.exec(str)
        if (match === null) {return [] }
    }
    return results
}



function readForm(reader: Reader): MalType {
    let curToken = reader.peek()
    switch (curToken) {
            case null:
                return new MalList([])
            case "(": 
                return readList(reader)
            default:
                return readAtom(reader)
    }
}

function readAtom(reader: Reader): MalAtom {
    // TODO: Add true, false and string
    // TODO: do symbols need to be refined further?
    if (reader.peek() === null) {
        throw new Error("Mismatched parenthesis, expected \")\"")
    }
    const token = reader.next() as string

    const numRe = /^(?=.)([+-]?([0-9]*)(\.([0-9]+))?)$/gm
    const match = numRe.exec(token)

    if (token.trim() === "nil") {
        return new MalNil()
    }
    // token === "+" or "-" because numRe erroneously captures "+" and "-"
    // TODO: fix regex
    else if (match === null || match[1] === '' || token === '+' || token === '-') {
         return new MalSymbol(token)
    } else {
        // cast to number
        const numToken: number = +token 
        return new MalNumber(numToken)
    }
}

function readList(reader: Reader): MalList {
    const mal: MalList = new MalList([])
    let curToken = reader.next()

    while ((curToken = reader.peek()) !== ")") {
        if (curToken === "(") {
            // process subList
            const subList = readList(reader)
            mal.push(subList)
        } else if (curToken === null) {
            throw new Error("Mismatched parenthesis, expected \")\"")        
        } else {
            // read Atom
            const atom = readAtom(reader)
            mal.push(atom)
        }
    }
    reader.next()
    return mal
}

export function readStr(str: string): MalType {
    const tokens = tokenize(str)
    const reader = new Reader(tokens)
    return readForm(reader)
}

