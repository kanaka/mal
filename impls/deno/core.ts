import * as Env from "./env.ts";
import * as MalType from "./types.ts";
import * as Printer from "./printer.ts";
import * as Reader from "./reader.ts";
import { readline } from "./readline.ts";

const mkHashPairs = (
  items: Array<MalType.MalType>,
  procedureName: string,
): Array<[MalType.MalType, MalType.MalType]> => {
  const args: Array<[MalType.MalType, MalType.MalType]> = [];

  if (items.length % 2 === 1) {
    throw new Error(
      `Invalid Argument: ${procedureName}: Odd number of arguments`,
    );
  }
  for (let lp = 0; lp < items.length; lp += 2) {
    args.push([items[lp], items[lp + 1]]);
  }

  return args;
};

const validateArgument = (
  o: number,
  v: MalType.MalType | undefined,
  tags: Array<string> | string | undefined = undefined,
) => {
  if (v === undefined) {
    throw { error: "UndefinedParameter", parameter: o, tags };
  }

  if (tags !== undefined) {
    if (Array.isArray(tags)) {
      if (!tags.includes(v.tag)) {
        throw { error: "InvalidTag", parameter: o, value: v, tags };
      }
    } else if (tags !== v.tag) {
      throw { error: "InvalidTag", parameter: o, value: v, tags: [tags] };
    }
  }
};

const __ns = (
  evaluate: (ast: MalType.MalType, env: Env.Env) => MalType.MalType,
) => ({
  "+": ([a, b]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, a, "MalNumber");
    validateArgument(1, b, "MalNumber");

    return MalType.mkNumber(asNumber(a) + asNumber(b));
  },

  "-": ([a, b]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, a, "MalNumber");
    validateArgument(1, b, "MalNumber");

    return MalType.mkNumber(asNumber(a) - asNumber(b));
  },

  "*": ([a, b]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, a, "MalNumber");
    validateArgument(1, b, "MalNumber");

    return MalType.mkNumber(asNumber(a) * asNumber(b));
  },

  "/": ([a, b]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, a, "MalNumber");
    validateArgument(1, b, "MalNumber");

    return MalType.mkNumber(asNumber(a) / asNumber(b));
  },

  "=": ([a, b]: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkBoolean(MalType.equals(a, b)),

  "<": ([a, b]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, a, "MalNumber");
    validateArgument(1, b, "MalNumber");

    return MalType.mkBoolean(asNumber(a) < asNumber(b));
  },

  "<=": ([a, b]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, a, "MalNumber");
    validateArgument(1, b, "MalNumber");

    return MalType.mkBoolean(asNumber(a) <= asNumber(b));
  },

  ">": ([a, b]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, a, "MalNumber");
    validateArgument(1, b, "MalNumber");

    return MalType.mkBoolean(asNumber(a) > asNumber(b));
  },

  ">=": ([a, b]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, a, "MalNumber");
    validateArgument(1, b, "MalNumber");

    return MalType.mkBoolean(asNumber(a) >= asNumber(b));
  },

  apply: ([f, ...rest]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, f, ["MalFunction", "MalInternalFunction"]);

    const items: Array<MalType.MalType> = [];

    rest.forEach((v) => {
      if (v.tag === "MalList" || v.tag == "MalVector") {
        v.items.forEach((i) => items.push(i));
      } else {
        items.push(v);
      }
    });

    if (f.tag === "MalInternalFunction") {
      return f.fn(items);
    } else if (f.tag === "MalFunction") {
      const ast = f.body;
      const env = Env.mkEnv(f.env, f.params, items);
      return evaluate(ast, env);
    } else {
      return MalType.nil;
    }
  },

  assoc: ([m, ...items]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, m, "MalHashMap");

    return MalType.mapAssoc(asHashMap(m), mkHashPairs(items ?? [], "assoc"));
  },

  atom: ([v]: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkAtom(v ?? MalType.nil),

  "atom?": ([v]: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkBoolean(v !== undefined && v.tag === "MalAtom"),

  concat: (lst: Array<MalType.MalType>): MalType.MalType => {
    const result: Array<MalType.MalType> = [];

    lst.forEach((v, i) => {
      validateArgument(i, v, ["MalNil", "MalList", "MalVector"]);
      if (v.tag !== "MalNil") {
        asSeq(v).items.forEach((e) => result.push(e));
      }
    });

    return MalType.mkList(result);
  },

  conj: ([c, ...es]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, c, ["MalList", "MalVector"]);

    return (es === undefined)
      ? c
      : (c.tag === "MalList")
      ? MalType.mkList([...es.reverse(), ...c.items])
      : MalType.mkVector([...asSeq(c).items, ...es]);
  },

  cons: ([a, b]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, a);
    validateArgument(1, b, ["MalNil", "MalList", "MalVector"]);

    return MalType.mkList([a, ...asSeq(b).items]);
  },

  "contains?": ([m, key]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, m, "MalHashMap");
    validateArgument(1, key, ["MalKeyword", "MalString"]);

    return MalType.mapContains(asHashMap(m), key);
  },

  count: ([v]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, v, ["MalList", "MalVector", "MalNil"]);

    return (v.tag === "MalNil")
      ? MalType.mkNumber(0)
      : MalType.mkNumber(asSeq(v).items.length);
  },

  deref: ([v]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, v, "MalAtom");

    return asAtom(v).value;
  },

  dissoc: ([m, ...items]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, m, "MalHashMap");

    return MalType.mapDissoc(asHashMap(m), items);
  },

  "empty?": ([v]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, v, ["MalList", "MalVector"]);

    return MalType.mkBoolean(asSeq(v).items.length === 0);
  },

  "false?": ([v]: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkBoolean(v !== undefined && v.tag === "MalBoolean" && !v.value),

  first: ([v]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, v, ["MalList", "MalVector", "MalNil"]);

    return v.tag === "MalNil" ? MalType.nil : asSeq(v).items[0] ?? MalType.nil;
  },

  "fn?": ([v]: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkBoolean(
      v !== undefined &&
        (v.tag === "MalFunction" && !v.isMacro ||
          v.tag === "MalInternalFunction"),
    ),

  get: ([m, key]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, m, ["MalHashMap", "MalNil"]);
    validateArgument(1, key, ["MalKeyword", "MalString"]);

    return m.tag === "MalNil" ? MalType.nil : MalType.mapGet(asHashMap(m), key);
  },

  "hash-map": (items: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkHashMap(mkHashPairs(items, "hash-map")),

  keys: ([m]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, m, "MalHashMap");

    return MalType.mkList(MalType.mapKeys(asHashMap(m)));
  },

  keyword: ([v]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, v, ["MalKeyword", "MalString"]);

    return (v.tag === "MalKeyword") ? v : MalType.mkKeyword(`:${asString(v)}`);
  },

  "keyword?": ([v]: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkBoolean(v !== undefined && v.tag === "MalKeyword"),

  list: MalType.mkList,

  "list?": ([v]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, v);

    return MalType.mkBoolean(v.tag === "MalList");
  },

  "macro?": ([v]: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkBoolean(v !== undefined && v.tag === "MalFunction" && v.isMacro),

  map: ([f, seq]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, f, ["MalFunction", "MalInternalFunction"]);
    validateArgument(1, seq, ["MalList", "MalVector"]);

    const fn = f.tag === "MalFunction"
      ? (args: Array<MalType.MalType>): MalType.MalType =>
        evaluate(f.body, Env.mkEnv(f.env, f.params, args))
      : asInternalFunction(f).fn;

    const mappedItems = asSeq(seq).items.map((p) => fn([p]));

    return MalType.mkList(mappedItems);
  },

  "map?": ([v]: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkBoolean(v !== undefined && v.tag === "MalHashMap"),

  meta: ([v]: Array<MalType.MalType>): MalType.MalType => {
    if (v === undefined) {
      return MalType.nil;
    } else {
      switch (v.tag) {
        case "MalFunction":
        case "MalHashMap":
        case "MalInternalFunction":
        case "MalVector":
        case "MalList":
          return v.meta ?? MalType.nil;
        default:
          return MalType.nil;
      }
    }
  },

  "nil?": ([v]: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkBoolean(v !== undefined && v.tag === "MalNil"),

  nth: ([l, i]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, l, ["MalList", "MalVector", "MalNil"]);
    validateArgument(1, i, "MalNumber");

    const result = asSeq(l).items[asNumber(i)];
    if (result === undefined) {
      throw new Error(
        `Index Out Of Range: nth: ${asNumber(i)} exceeds bounds of ${
          JSON.stringify(l)
        }`,
      );
    } else {
      return result;
    }
  },

  "number?": ([v]: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkBoolean(v !== undefined && v.tag === "MalNumber"),

  "pr_str": ([v]: Array<MalType.MalType>): MalType.MalType => {
    console.log(Printer.prStr(v, true));
    return MalType.nil;
  },

  println: (args: Array<MalType.MalType>): MalType.MalType => {
    console.log(args.map((v) => Printer.prStr(v, false)).join(" "));
    return MalType.nil;
  },

  "pr-str": (args: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkString(args.map((v) => Printer.prStr(v, true)).join(" ")),

  prn: (args: Array<MalType.MalType>): MalType.MalType => {
    console.log(args.map((v) => Printer.prStr(v, true)).join(" "));
    return MalType.nil;
  },

  "read-string": ([s]: Array<MalType.MalType>): MalType.MalType => {
    if (s === undefined) {
      return MalType.nil;
    }

    validateArgument(0, s, "MalString");

    return Reader.readStr(asString(s));
  },

  readline: ([prompt]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, prompt, "MalString");

    const text = readline(`${asString(prompt)}> `);

    return text === undefined ? MalType.nil : MalType.mkString(text);
  },

  "reset!": ([a, v]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, a, "MalAtom");

    asAtom(a).value = v ?? MalType.nil;

    return asAtom(a).value;
  },

  rest: ([v]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, v, ["MalList", "MalVector", "MalNil"]);

    return v.tag === "MalNil"
      ? MalType.mkList([])
      : MalType.mkList(asSeq(v).items.slice(1));
  },

  seq: ([s]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, s, ["MalList", "MalVector", "MalNil", "MalString"]);

    if (s.tag === "MalList") {
      return s.items.length === 0 ? MalType.nil : s;
    } else if (s.tag === "MalVector") {
      return s.items.length === 0 ? MalType.nil : MalType.mkList(s.items);
    } else if (s.tag === "MalNil") {
      return s;
    } else {
      const sp = asString(s);

      return sp.length === 0
        ? MalType.nil
        : MalType.mkList(sp.split("").map((e) => MalType.mkString(e)));
    }
  },

  "sequential?": ([v]: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkBoolean(
      v !== undefined && (v.tag === "MalVector" || v.tag === "MalList"),
    ),

  slurp: ([s]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, s, "MalString");

    return MalType.mkString(Deno.readTextFileSync(asString(s)));
  },

  str: (args: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkString(args.map((v) => Printer.prStr(v, false)).join("")),

  symbol: ([v]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, v, ["MalSymbol", "MalString"]);

    return (v.tag === "MalSymbol") ? v : MalType.mkSymbol(asString(v));
  },

  "string?": ([v]: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkBoolean(v !== undefined && v.tag === "MalString"),

  "symbol?": ([v]: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkBoolean(v !== undefined && v.tag === "MalSymbol"),

  "swap!": ([a, f, ...args]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, a, "MalAtom");
    validateArgument(1, f, ["MalFunction", "MalInternalFunction"]);

    const ap = asAtom(a);

    args = [ap.value, ...(args ?? [])];

    if (f.tag === "MalFunction") {
      ap.value = evaluate(f.body, Env.mkEnv(f.env, f.params, args));
    } else if (f.tag === "MalInternalFunction") {
      ap.value = f.fn(args);
    }

    return ap.value;
  },

  throw: ([v]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, v);

    throw v;
  },

  "time-ms": (_: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkNumber(performance.now()),

  "true?": ([v]: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkBoolean(v !== undefined && v.tag === "MalBoolean" && v.value),

  vals: ([m]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, m, "MalHashMap");

    return MalType.mkList(MalType.mapValues(asHashMap(m)));
  },

  vec: ([v]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, v, ["MalList", "MalVector"]);

    return (v.tag === "MalVector") ? v : MalType.mkVector(asSeq(v).items);
  },

  vector: MalType.mkVector,

  "vector?": ([v]: Array<MalType.MalType>): MalType.MalType =>
    MalType.mkBoolean(v !== undefined && v.tag === "MalVector"),

  "with-meta": ([v, m]: Array<MalType.MalType>): MalType.MalType => {
    validateArgument(0, v);
    validateArgument(1, m);

    return MalType.withMeta(v, m);
  },
});

export const ns = (
  evaluate: (ast: MalType.MalType, env: Env.Env) => MalType.MalType,
): Array<[string, MalType.MalType]> => {
  const fs: any = __ns(evaluate);

  const mkFun = (
    name: string,
    fn: (args: Array<MalType.MalType>) => MalType.MalType,
  ): MalType.MalInternalFunction => {
    const fnp = (args: Array<MalType.MalType>): MalType.MalType => {
      try {
        return fn(args);
      } catch (e: any) {
        if (e.error === "UndefinedParameter") {
          throw MalType.mkList([
            MalType.mkSymbol("UndefinedParameter"),
            MalType.mkSymbol(name),
            MalType.mkNumber(e.parameter),
          ]);
        } else if (e.error === "InvalidTag") {
          throw MalType.mkList([
            MalType.mkSymbol("InvalidParameter"),
            MalType.mkSymbol(name),
            MalType.mkNumber(e.parameter),
            e.value,
            MalType.mkList(e.tags.map(MalType.mkSymbol)),
          ]);
        } else {
          throw e;
        }
      }
    };

    return MalType.mkInternalFunction(fnp);
  };

  return Object.keys(fs).map((
    key: string,
  ) => [key, mkFun(key, fs[key])]);
};

const asAtom = (v: MalType.MalType): MalType.MalAtom => {
  if (v.tag === "MalAtom") {
    return v;
  } else {
    throw Error("Unable to coerce to atom");
  }
};

const asHashMap = (v: MalType.MalType): MalType.MalHashMap => {
  if (v.tag === "MalHashMap") {
    return v;
  } else {
    throw Error("Unable to coerce to hashmap");
  }
};

const asInternalFunction = (
  v: MalType.MalType,
): MalType.MalInternalFunction => {
  if (v.tag === "MalInternalFunction") {
    return v;
  } else {
    throw Error("Unable to coerce to internal function");
  }
};

const asNumber = (v: MalType.MalType): number => {
  if (v.tag === "MalNumber") {
    return v.value;
  } else {
    throw Error("Unable to coerce to number");
  }
};

const asSeq = (v: MalType.MalType): MalType.MalList | MalType.MalVector => {
  if (v.tag === "MalList" || v.tag === "MalVector") {
    return v;
  } else {
    throw Error("Unable to coerce to sequence");
  }
};

const asString = (v: MalType.MalType): string => {
  if (v.tag === "MalString") {
    return v.value;
  } else {
    throw Error("Unable to coerce to string");
  }
};
