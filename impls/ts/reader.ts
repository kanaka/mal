import { MalType, MalList, MalString, MalNumber, MalBoolean, MalNil, MalKeyword, MalSymbol, MalVector, MalHashMap } from "./types";

class Reader {
    position = 0;

    constructor(private tokens: string[]) { }

    next(): string {
        const ret = this.peek();
        this.position += 1;
        return ret;
    }

    peek(): string {
        return this.tokens[this.position];
    }
}

export function readStr(input: string): MalType {
    const tokens = tokenizer(input);
    const reader = new Reader(tokens);
    return readForm(reader);
}

function tokenizer(input: string): string[] {
    const regexp = /[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)/g;
    const tokens: string[] = [];
    while (true) {
        const matches = regexp.exec(input);
        if (!matches) {
            break;
        }
        const match = matches[1];
        if (match === "") {
            break;
        }
        if (match[0] !== ";") {
            tokens.push(match);
        }
    }

    return tokens;
}

function readForm(reader: Reader): MalType {
    const token = reader.peek();
    switch (token) {
        case "(":
            return readList(reader);
        case "[":
            return readVector(reader);
        case "{":
            return readHashMap(reader);
        case "'":
            return readSymbol("quote");
        case "`":
            return readSymbol("quasiquote");
        case "~":
            return readSymbol("unquote");
        case "~@":
            return readSymbol("splice-unquote");
        case "@":
            return readSymbol("deref");
        case "^":
            {
                reader.next();
                const sym = MalSymbol.get("with-meta");
                const target = readForm(reader);
                return new MalList([sym, readForm(reader), target]);
            }
        default:
            return readAtom(reader);
    }

    function readSymbol(name: string) {
        reader.next();
        const sym = MalSymbol.get(name);
        const target = readForm(reader);
        return new MalList([sym, target]);
    }
}

function readList(reader: Reader): MalType {
    return readParen(reader, MalList, "(", ")");
}

function readVector(reader: Reader): MalType {
    return readParen(reader, MalVector, "[", "]");
}

function readHashMap(reader: Reader): MalType {
    return readParen(reader, MalHashMap, "{", "}");
}

function readParen(reader: Reader, ctor: { new (list: MalType[]): MalType; }, open: string, close: string): MalType {
    const token = reader.next(); // drop open paren
    if (token !== open) {
        throw new Error(`unexpected token ${token}, expected ${open}`);
    }
    const list: MalType[] = [];
    while (true) {
        const next = reader.peek();
        if (next === close) {
            break;
        } else if (!next) {
            throw new Error("unexpected EOF");
        }
        list.push(readForm(reader));
    }
    reader.next(); // drop close paren

    return new ctor(list);
}

function readAtom(reader: Reader): MalType {
    const token = reader.next();
    if (token.match(/^-?[0-9]+$/)) {
        const v = parseInt(token, 10);
        return new MalNumber(v);
    }
    if (token.match(/^-?[0-9]\.[0-9]+$/)) {
        const v = parseFloat(token);
        return new MalNumber(v);
    }
    if (token.match(/^"(?:\\.|[^\\"])*"$/)) {
        const v = token.slice(1, token.length - 1)
            .replace(/\\(.)/g, (_, c: string) => c == 'n' ? '\n' : c)
        return new MalString(v);
    }
    if (token[0] === '"') {
        throw new Error("expected '\"', got EOF");
    }
    if (token[0] === ":") {
        return MalKeyword.get(token.substr(1));
    }
    switch (token) {
        case "nil":
            return MalNil.instance;
        case "true":
            return new MalBoolean(true);
        case "false":
            return new MalBoolean(false);
    }

    return MalSymbol.get(token);
}
