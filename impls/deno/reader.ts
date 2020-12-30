import {
  MalAtom,
  MalList,
  MalNumber,
  MalType,
  MalVector,
  mkMalMap,
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
  const tokenPattern =
    /[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)/;

  const regex = new RegExp(tokenPattern, "g");

  const tokens = [];
  while (true) {
    const tokenResults = regex.exec(input);
    if (tokenResults === null || tokenResults[0] === "") {
      break;
    } else if (tokenResults[1] !== ";") {
      tokens.push(tokenResults[1]);
    }
  }

  return tokens;
};

const readForm = (reader: Reader): MalType => {
  const token = reader.peek();

  switch (token) {
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
  readCollection(
    reader,
    ")",
    (items: Array<MalType>): MalType => ({ tag: "MalList", items }),
  );

const readVector = (reader: Reader): MalType =>
  readCollection(
    reader,
    "]",
    (items: Array<MalType>): MalType => ({ tag: "MalVector", items }),
  );

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

    return mkMalMap(args);
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

  return {
    tag: "MalList",
    items: [{ tag: "MalSymbol", name }, readForm(reader)],
  };
};

const readMetaData = (reader: Reader): MalType => {
  reader.next();

  const v1 = readForm(reader);
  const v2 = readForm(reader);

  return {
    tag: "MalList",
    items: [{ tag: "MalSymbol", name: "with-meta" }, v2, v1],
  };
};

const readAtom = (reader: Reader): MalType => {
  const token = reader.next();

  if (token === undefined) {
    throw new Error(`Syntax Error: Unexpected EOF`);
  } else if (token.match(/^[0-9]+$/)) {
    return { tag: "MalNumber", value: parseInt(token) };
  } else if (token == "false") {
    return { tag: "MalBoolean", value: false };
  } else if (token == "true") {
    return { tag: "MalBoolean", value: true };
  } else if (token == "nil") {
    return { tag: "MalNil" };
  } else if (token[0] === '"') {
    if (token.match(/^"(?:\\.|[^\\"])*"$/)) {
      return {
        tag: "MalString",
        value: token.substr(1, token.length - 2).replaceAll("\\\\", "\\")
          .replaceAll('\\"', '"').replaceAll("\\n", "\n"),
      };
    } else {
      throw new Error(`Syntax Error: EOF whilst expecting '"': ${token}`);
    }
  } else if (token[0] === ":") {
    return { tag: "MalSymbol", name: token };
  } else {
    return { tag: "MalAtom", value: token };
  }
};
