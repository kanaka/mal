import {
  MalNumber,
  MalType,
  MalVector,
  mkBoolean,
  mkHashMap,
  mkKeyword,
  mkList,
  mkNumber,
  mkString,
  mkSymbol,
  mkVector,
  nil,
} from "./types.ts";

interface Reader {
  position: number;
  peek: () => string;
  next: () => string;
}

const newReader = (tokens: Array<string>): Reader => ({
  position: 0,

  peek: function (): string {
    return tokens[this.position];
  },

  next: function (): string {
    const current = this.peek();
    this.position += 1;
    return current;
  },
});

export const readStr = (input: string): MalType =>
  readForm(newReader(tokenize(input)));

const tokenize = (input: string): Array<string> => {
  const regex =
    /[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)/g;

  const tokens = [];
  while (true) {
    const tokenResults = regex.exec(input);
    if (tokenResults === null || tokenResults[0] === "") {
      break;
    } else if (tokenResults[1][0] !== ";") {
      tokens.push(tokenResults[1]);
    }
  }

  return tokens;
};

const readForm = (reader: Reader): MalType => {
  const token = reader.peek();

  switch (token) {
    case undefined:
      throw new Error("Reader Error: No input");
    case "(":
      return readList(reader);
    case "[":
      return readVector(reader);
    case "{":
      return readHashMap(reader);
    case "'":
      return readSymbol(reader, "quote");
    case "`":
      return readSymbol(reader, "quasiquote");
    case "~":
      return readSymbol(reader, "unquote");
    case "~@":
      return readSymbol(reader, "splice-unquote");
    case "@":
      return readSymbol(reader, "deref");
    case "^":
      return readMetaData(reader);
    case ")":
    case "]":
    case "}":
      throw new Error(`Syntax Error: Unexpected '${token}'`);
    default:
      return readAtom(reader);
  }
};

const readList = (reader: Reader): MalType =>
  readCollection(reader, ")", mkList);

const readVector = (reader: Reader): MalType =>
  readCollection(reader, "]", mkVector);

const readHashMap = (
  reader: Reader,
): MalType => {
  const buildMap = (items: Array<MalType>): MalType => {
    const args: Array<[MalType, MalType]> = [];

    if (items.length % 2 === 1) {
      throw new Error(`Syntax Error: Odd number of map arguments`);
    }
    for (let lp = 0; lp < items.length; lp += 2) {
      args.push([items[lp], items[lp + 1]]);
    }

    return mkHashMap(args);
  };

  return readCollection(reader, "}", buildMap);
};

const readCollection = (
  reader: Reader,
  close: string,
  mk: (a: Array<MalType>) => MalType,
): MalType => {
  reader.next();

  const items: Array<MalType> = [];

  while (true) {
    const token = reader.peek();

    if (token === undefined) {
      throw new Error(`Syntax Error: EOF whilst expecting '${close}'`);
    } else if (token === close) {
      reader.next();
      return mk(items);
    } else {
      items.push(readForm(reader));
    }
  }
};

const readSymbol = (reader: Reader, name: string): MalType => {
  reader.next();

  return mkList([mkSymbol(name), readForm(reader)]);
};

const readMetaData = (reader: Reader): MalType => {
  reader.next();

  const v1 = readForm(reader);
  const v2 = readForm(reader);

  return mkList([mkSymbol("with-meta"), v2, v1]);
};

const readAtom = (reader: Reader): MalType => {
  const token = reader.next();

  if (token === undefined) {
    throw new Error(`Syntax Error: Unexpected EOF`);
  } else if (token.match(/^-?[0-9]+$/)) {
    return mkNumber(parseInt(token));
  } else if (token == "false") {
    return mkBoolean(false);
  } else if (token == "true") {
    return mkBoolean(true);
  } else if (token == "nil") {
    return nil;
  } else if (token[0] === '"') {
    if (token.match(/^"(?:\\.|[^\\"])*"$/)) {
      return mkString(
        token.substr(1, token.length - 2).replace(
          /\\(.)/g,
          (_, c: string) => c == "n" ? "\n" : c,
        ),
      );
    } else {
      throw new Error(`Syntax Error: EOF whilst expecting '"': ${token}`);
    }
  } else if (token[0] === ":") {
    return mkKeyword(token);
  } else {
    return mkSymbol(token);
  }
};
