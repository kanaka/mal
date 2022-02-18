import { MalType, MalList, MalNumber, MalSymbol, MalNil, MalAtom, MalBoolean, MalString, keywordPrefix, MalMap, MalTypes } from "./types";


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
            case "[":
                return readList(reader, curToken)
            case "{":
                return readMap(reader)
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
    const trimmedToken = token.trim()

    if (trimmedToken === "nil") {
        return new MalNil()
    } else if (trimmedToken === "true" ) {
        return new MalBoolean(true)
    } else if (trimmedToken === "false") {
        return new MalBoolean(false)
    } else if (trimmedToken.startsWith('"')) {
        if (!trimmedToken.endsWith('"')) {
            throw new Error("Expected \" at the end of string")
        }
        // remove "" from the start and end of token
        return new MalString(trimmedToken.slice(1, trimmedToken.length-1))
    } else if (trimmedToken.startsWith(":")) {
        // add special character ʞ to denote this string is actually a keyword
        // the printer converts this back into :abc form
        return new MalString(keywordPrefix + trimmedToken.slice(1))
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

function readList(reader: Reader, delimiter: string): MalList {
    const endDelimiter = delimiter === "[" ? "]" : ")"
    const isVector = delimiter === "[" 
    const mal = new MalList([], isVector) 
    let curToken = reader.next()
    while ((curToken = reader.peek())!== endDelimiter) {
        if (curToken === "(" || curToken === "[") {
            // process subList
            const subList = readList(reader, curToken)
            mal.push(subList)
        } else if (curToken === null) {
            throw new Error(`Mismatched parenthesis, expected ${endDelimiter}`)        
        } else {
            // read Atom
            const atom = readAtom(reader)
            mal.push(atom)
        }
    }
    reader.next()
    return mal
}

function readMap(reader: Reader): MalMap {
    const map = new MalMap()
    let curToken = reader.next() // consume "{"
    let index = 0
    let curKey = new MalString("")   
    while ((curToken = reader.peek()) !== "}") {
        let curElement: MalType = new MalNil()
        if (curToken === "{") {
            // process subMap
            curElement = readMap(reader)
        } else if (curToken === "(" || curToken === "[") {
            curElement = readList(reader, curToken)
        } else if (curToken === null) {
            throw new Error(`Mismatched parenthesis, expected "}"`)        
        } else {
            // read atom
            curElement = readAtom(reader)
        }
        if (index % 2 == 0) {
            // key
            if (curElement.type !== MalTypes.String) 
                throw new Error(`Map keys can only be of type MalString. Unsupported type ${curElement.type}`)
            curKey = curElement as MalString
        } else {
            // value
            map.map.set(curKey, curElement)
        }
        index++
    }
    if (index % 2 == 1){
        // index odd => some values missing
        console.log(index)
        throw new Error("Missing value for key.")
    }

    reader.next() // consume "}"
    return map
}

export function readStr(str: string): MalType {
    const tokens = tokenize(str)
    const reader = new Reader(tokens)
    return readForm(reader)
}

