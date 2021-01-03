import * as Env from "./env.ts";
import * as MalType from "./types.ts";
import * as Printer from "./printer.ts";
import * as Reader from "./reader.ts";
import { readline } from "./readline.ts";

export const ns = (
  evaluate: (ast: MalType.MalType, env: Env.Env) => MalType.MalType,
): Array<[string, MalType.MalType]> => [
  [
    "+",
    MalType.mkInternalFunction(([a, b]) =>
      MalType.mkNumber(MalType.asNumber(a) + MalType.asNumber(b))
    ),
  ],
  [
    "-",
    MalType.mkInternalFunction(([a, b]) =>
      MalType.mkNumber(MalType.asNumber(a) - MalType.asNumber(b))
    ),
  ],
  [
    "*",
    MalType.mkInternalFunction(([a, b]) =>
      MalType.mkNumber(MalType.asNumber(a) * MalType.asNumber(b))
    ),
  ],
  [
    "/",
    MalType.mkInternalFunction(([a, b]) =>
      MalType.mkNumber(MalType.asNumber(a) / MalType.asNumber(b))
    ),
  ],

  [
    "pr_str",
    MalType.mkInternalFunction(([v]) => {
      console.log(Printer.prStr(v, true));
      return MalType.nil;
    }),
  ],
  [
    "println",
    MalType.mkInternalFunction((args) => {
      console.log(args.map((v) => Printer.prStr(v, false)).join(" "));
      return MalType.nil;
    }),
  ],
  [
    "prn",
    MalType.mkInternalFunction((args) => {
      console.log(args.map((v) => Printer.prStr(v, true)).join(" "));
      return MalType.nil;
    }),
  ],
  [
    "pr-str",
    MalType.mkInternalFunction((args) =>
      MalType.mkString(args.map((v) => Printer.prStr(v, true)).join(" "))
    ),
  ],
  [
    "str",
    MalType.mkInternalFunction((args) =>
      MalType.mkString(args.map((v) => Printer.prStr(v, false)).join(""))
    ),
  ],
  ["list", MalType.mkInternalFunction((v) => MalType.mkList(v))],
  [
    "list?",
    MalType.mkInternalFunction(([v]) => {
      if (v === undefined) {
        throw new Error("Illegal Argument: list? requires a parameter");
      }
      return MalType.mkBoolean(v.tag === "MalList");
    }),
  ],
  [
    "empty?",
    MalType.mkInternalFunction(([v]) => {
      if (v === undefined || v.tag !== "MalList" && v.tag !== "MalVector") {
        throw new Error(
          "Illegal Argument: empty? requires a list or vector as parameter",
        );
      }
      return MalType.mkBoolean(v.items.length === 0);
    }),
  ],
  [
    "count",
    MalType.mkInternalFunction(([v]) => {
      if (v.tag === "MalNil") {
        return MalType.mkNumber(0);
      }

      if (v.tag !== "MalList" && v.tag !== "MalVector") {
        throw new Error(
          "Illegal Argument: count requires a list or vector parameter",
        );
      }
      return MalType.mkNumber(v.items.length);
    }),
  ],

  [
    "=",
    MalType.mkInternalFunction(([a, b]) =>
      MalType.mkBoolean(MalType.equals(a, b))
    ),
  ],
  [
    "<",
    MalType.mkInternalFunction(([a, b]) =>
      MalType.mkBoolean(MalType.asNumber(a) < MalType.asNumber(b))
    ),
  ],
  [
    "<=",
    MalType.mkInternalFunction(([a, b]) =>
      MalType.mkBoolean(MalType.asNumber(a) <= MalType.asNumber(b))
    ),
  ],
  [
    ">",
    MalType.mkInternalFunction(([a, b]) =>
      MalType.mkBoolean(MalType.asNumber(a) > MalType.asNumber(b))
    ),
  ],
  [
    ">=",
    MalType.mkInternalFunction(([a, b]) =>
      MalType.mkBoolean(MalType.asNumber(a) >= MalType.asNumber(b))
    ),
  ],

  [
    "read-string",
    MalType.mkInternalFunction(([s]) => {
      if (s === undefined) {
        return MalType.nil;
      } else if (s.tag === "MalString") {
        return Reader.readStr(s.value);
      } else {
        throw new Error(
          `Invalid Argument: read-string requires a string argument: ${
            JSON.stringify(s)
          }`,
        );
      }
    }),
  ],
  [
    "slurp",
    MalType.mkInternalFunction(([s]) => {
      if (s === undefined) {
        throw new Error(
          `Invalid Argument: slurp requires a single string argument`,
        );
      } else if (s.tag === "MalString") {
        return MalType.mkString(Deno.readTextFileSync(s.value));
      } else {
        throw new Error(
          `Invalid Argument: slurp requires a string argument: ${
            JSON.stringify(s)
          }`,
        );
      }
    }),
  ],

  [
    "atom",
    MalType.mkInternalFunction(([v]) => {
      return MalType.mkAtom(v ?? MalType.nil);
    }),
  ],
  [
    "atom?",
    MalType.mkInternalFunction(([v]) => {
      return MalType.mkBoolean(v !== undefined && v.tag === "MalAtom");
    }),
  ],
  [
    "deref",
    MalType.mkInternalFunction(([v]) => {
      if (v === undefined) {
        throw new Error(
          `Invalid Argument: deref requires a single atom argument`,
        );
      } else if (v.tag === "MalAtom") {
        return v.value;
      } else {
        throw new Error(
          `Invalid Argument: deref requires an atom argument: ${
            JSON.stringify(v)
          }`,
        );
      }
    }),
  ],
  [
    "reset!",
    MalType.mkInternalFunction(([a, v]) => {
      if (a === undefined) {
        throw new Error(`Invalid Argument: reset! requires an atom argument`);
      } else if (a.tag !== "MalAtom") {
        throw new Error(
          `Invalid Argument: reset! requires an atom argument: ${
            JSON.stringify(a)
          }`,
        );
      }
      a.value = v ?? MalType.nil;

      return a.value;
    }),
  ],
  [
    "swap!",
    MalType.mkInternalFunction(([a, f, ...args]) => {
      if (a === undefined) {
        throw new Error(`Invalid Argument: swap! requires an atom argument`);
      } else if (a.tag !== "MalAtom") {
        throw new Error(
          `Invalid Argument: swap! requires an atom argument: ${
            JSON.stringify(a)
          }`,
        );
      }

      if (f === undefined) {
        throw new Error(
          `Invalid Argument: swap! requires an function argument`,
        );
      } else if (f.tag !== "MalFunction" && f.tag !== "MalInternalFunction") {
        throw new Error(
          `Invalid Argument: swap! requires a function argument: ${
            JSON.stringify(a)
          }`,
        );
      }

      args = [a.value, ...(args ?? [])];

      if (f.tag === "MalFunction") {
        a.value = evaluate(f.body, Env.mkEnv(f.env, f.params, args));
      } else {
        a.value = f.fn(args);
      }

      return a.value;
    }),
  ],

  [
    "cons",
    MalType.mkInternalFunction(([a, b]) => {
      if (a === undefined || b === undefined) {
        throw new Error(`Invalid Argument: cons requires two arguments`);
      }

      if (b.tag === "MalNil") {
        return MalType.mkList([a]);
      } else if (b.tag !== "MalList" && b.tag !== "MalVector") {
        throw new Error(
          `Invalid Argument: cons second argument must be a list: ${
            JSON.stringify(b)
          }`,
        );
      }

      return MalType.mkList([a, ...b.items]);
    }),
  ],
  [
    "concat",
    MalType.mkInternalFunction((lst) => {
      const result: Array<MalType.MalType> = [];

      lst.forEach((i) => {
        if (i.tag === "MalNil") {
          // do nothing
        } else if (i.tag === "MalList" || i.tag === "MalVector") {
          i.items.forEach((e) => result.push(e));
        } else {
          throw new Error(
            `Invalid Argument: concat argument must be a list: ${
              JSON.stringify(i)
            }`,
          );
        }
      });

      return MalType.mkList(result);
    }),
  ],
  [
    "vec",
    MalType.mkInternalFunction(([v]) => {
      if (v === undefined) {
        throw new Error("Invalid Argument: vec requires a single argument");
      }

      if (v.tag === "MalVector") {
        return v;
      } else if (v.tag === "MalList") {
        return MalType.mkVector(v.items);
      } else {
        throw new Error(
          `Invalid Argument: vec requires a single list or vector argument: ${
            JSON.stringify(v)
          }`,
        );
      }
    }),
  ],
  [
    "nth",
    MalType.mkInternalFunction(([l, i]) => {
      if (l === undefined || l.tag !== "MalList" && l.tag !== "MalVector") {
        throw new Error(
          `Invalid Argument: nth parameter 0: expected list or vector: ${
            JSON.stringify(i)
          }`,
        );
      }
      if (i === undefined || i.tag !== "MalNumber") {
        throw new Error(
          `Invalid Argument: nth parameter 1: expected number: ${
            JSON.stringify(i)
          }`,
        );
      }

      const result = l.items[i.value];
      if (result === undefined) {
        throw new Error(
          `Index Out Of Range: nth: ${i.value} exceeds bounds of ${
            JSON.stringify(l)
          }`,
        );
      } else {
        return result;
      }
    }),
  ],
  [
    "first",
    MalType.mkInternalFunction(([v]) => {
      if (
        v === undefined ||
        v.tag !== "MalList" && v.tag !== "MalVector" && v.tag !== "MalNil"
      ) {
        throw new Error(
          "Invalid Argument: first parameter 0: expected list, vector or nil",
        );
      }

      return v.tag === "MalNil" ? MalType.nil : v.items[0] ?? MalType.nil;
    }),
  ],
  [
    "rest",
    MalType.mkInternalFunction(([v]) => {
      if (
        v === undefined ||
        v.tag !== "MalList" && v.tag !== "MalVector" && v.tag !== "MalNil"
      ) {
        throw new Error(
          "Invalid Argument: rest parameter 0: expected list, vector or nil",
        );
      }

      return v.tag === "MalNil"
        ? MalType.mkList([])
        : MalType.mkList(v.items.slice(1));
    }),
  ],

  [
    "throw",
    MalType.mkInternalFunction(([v]) => {
      if (
        v === undefined
      ) {
        throw new Error(
          "Invalid Argument: throw parameter 0: expected",
        );
      }

      throw v;
    }),
  ],

  [
    "apply",
    MalType.mkInternalFunction(([f, ...rest]) => {
      if (
        f === undefined ||
        f.tag !== "MalInternalFunction" && f.tag !== "MalFunction"
      ) {
        throw new Error(
          "Invalid Argument: apply parameter 0: expected function",
        );
      }

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
      } else {
        const ast = f.body;
        const env = Env.mkEnv(f.env, f.params, items);
        return evaluate(ast, env);
      }
    }),
  ],
  [
    "map",
    MalType.mkInternalFunction(([f, seq]) => {
      if (
        f === undefined ||
        f.tag !== "MalInternalFunction" && f.tag !== "MalFunction"
      ) {
        throw new Error(
          "Invalid Argument: map parameter 0: expected function",
        );
      }
      if (
        seq === undefined || seq.tag !== "MalList" && seq.tag !== "MalVector"
      ) {
        throw new Error(
          "Invalid Argument: map parameter 1: expected sequence",
        );
      }

      const fn = f.tag === "MalInternalFunction"
        ? f.fn
        : (args: Array<MalType.MalType>): MalType.MalType =>
          evaluate(f.body, Env.mkEnv(f.env, f.params, args));

      const mappedItems = seq.items.map((p) => fn([p]));

      return MalType.mkList(mappedItems);
    }),
  ],
  [
    "nil?",
    MalType.mkInternalFunction(([v]) => {
      return MalType.mkBoolean(v !== undefined && v.tag === "MalNil");
    }),
  ],
  [
    "true?",
    MalType.mkInternalFunction(([v]) => {
      return MalType.mkBoolean(
        v !== undefined && v.tag === "MalBoolean" && v.value,
      );
    }),
  ],
  [
    "false?",
    MalType.mkInternalFunction(([v]) => {
      return MalType.mkBoolean(
        v !== undefined && v.tag === "MalBoolean" && !v.value,
      );
    }),
  ],
  [
    "symbol?",
    MalType.mkInternalFunction(([v]) => {
      return MalType.mkBoolean(v !== undefined && v.tag === "MalSymbol");
    }),
  ],
  [
    "symbol",
    MalType.mkInternalFunction(([v]) => {
      if (v === undefined) {
        throw new Error(
          "Invalid Argument: symbol parameter 0: expected string or symbol",
        );
      }
      if (v.tag === "MalSymbol") {
        return v;
      } else if (v.tag === "MalString") {
        return MalType.mkSymbol(v.value);
      } else {
        throw new Error(
          `Invalid Argument: symbol parameter 0: expected string or symbol: ${
            JSON.stringify(v)
          }`,
        );
      }
    }),
  ],
  [
    "keyword",
    MalType.mkInternalFunction(([v]) => {
      if (v === undefined) {
        throw new Error(
          "Invalid Argument: symbol parameter 0: expected string or keyword",
        );
      }
      if (v.tag === "MalKeyword") {
        return v;
      } else if (v.tag === "MalString") {
        return MalType.mkKeyword(`:${v.value}`);
      } else {
        throw new Error(
          `Invalid Argument: keyword parameter 0: expected string or keyword: ${
            JSON.stringify(v)
          }`,
        );
      }
    }),
  ],
  [
    "keyword?",
    MalType.mkInternalFunction(([v]) => {
      return MalType.mkBoolean(v !== undefined && v.tag === "MalKeyword");
    }),
  ],
  [
    "string?",
    MalType.mkInternalFunction(([v]) => {
      return MalType.mkBoolean(v !== undefined && v.tag === "MalString");
    }),
  ],
  [
    "number?",
    MalType.mkInternalFunction(([v]) => {
      return MalType.mkBoolean(v !== undefined && v.tag === "MalNumber");
    }),
  ],
  [
    "vector",
    MalType.mkInternalFunction((lst) => {
      return MalType.mkVector(lst);
    }),
  ],
  [
    "vector?",
    MalType.mkInternalFunction(([v]) => {
      return MalType.mkBoolean(v !== undefined && v.tag === "MalVector");
    }),
  ],
  [
    "sequential?",
    MalType.mkInternalFunction(([v]) =>
      MalType.mkBoolean(
        v !== undefined && (v.tag === "MalVector" || v.tag === "MalList"),
      )
    ),
  ],
  [
    "hash-map",
    MalType.mkInternalFunction((items) =>
      MalType.mkHashMap(mkHashPairs(items, "hash-map"))
    ),
  ],
  [
    "map?",
    MalType.mkInternalFunction(([v]) =>
      MalType.mkBoolean(v !== undefined && v.tag === "MalHashMap")
    ),
  ],
  [
    "assoc",
    MalType.mkInternalFunction(([m, ...items]) => {
      if (m === undefined || m.tag !== "MalHashMap") {
        throw new Error(
          "Invalid Argument: assoc parameter 0: expected a hash map",
        );
      }
      return MalType.mapAssoc(m, mkHashPairs(items ?? [], "assoc"));
    }),
  ],
  [
    "dissoc",
    MalType.mkInternalFunction(([m, ...items]) => {
      if (m === undefined || m.tag !== "MalHashMap") {
        throw new Error(
          "Invalid Argument: dissoc parameter 0: expected a hash map",
        );
      }

      return MalType.mapDissoc(m, items);
    }),
  ],
  [
    "get",
    MalType.mkInternalFunction(([m, key]) => {
      if (m === undefined || m.tag !== "MalHashMap" && m.tag !== "MalNil") {
        throw new Error(
          "Invalid Argument: get parameter 0: expected a hash map",
        );
      }

      if (
        key === undefined ||
        (key.tag !== "MalKeyword" && key.tag !== "MalString")
      ) {
        throw new Error(
          "Invalid Argument: get parameter 1: expected a keyword or string",
        );
      }

      return m.tag === "MalNil" ? MalType.nil : MalType.mapGet(m, key);
    }),
  ],
  [
    "contains?",
    MalType.mkInternalFunction(([m, key]) => {
      if (m === undefined || m.tag !== "MalHashMap") {
        throw new Error(
          "Invalid Argument: contains? parameter 0: expected a hash map",
        );
      }
      if (
        key === undefined ||
        (key.tag !== "MalKeyword" && key.tag !== "MalString")
      ) {
        throw new Error(
          "Invalid Argument: contains? parameter 1: expected a keyword or string",
        );
      }

      return MalType.mapContains(m, key);
    }),
  ],
  [
    "keys",
    MalType.mkInternalFunction(([m]) => {
      if (m === undefined || m.tag !== "MalHashMap") {
        throw new Error(
          "Invalid Argument: contains? parameter 0: expected a hash map",
        );
      }

      return MalType.mkList(MalType.mapKeys(m));
    }),
  ],
  [
    "vals",
    MalType.mkInternalFunction(([m]) => {
      if (m === undefined || m.tag !== "MalHashMap") {
        throw new Error(
          "Invalid Argument: contains? parameter 0: expected a hash map",
        );
      }

      return MalType.mkList(MalType.mapValues(m));
    }),
  ],

  [
    "readline",
    MalType.mkInternalFunction(([prompt]) => {
      if (prompt === undefined || prompt.tag !== "MalString") {
        throw new Error(
          "Invalid Argument: readline parameter 0: expected a string",
        );
      }

      const text = readline(`${prompt.value}> `);

      return text === undefined ? MalType.nil : MalType.mkString(text);
    }),
  ],

  [
    "time-ms",
    MalType.mkInternalFunction((_) => MalType.mkNumber(performance.now())),
  ],
  [
    "meta",
    MalType.mkInternalFunction(([v]) => {
      if (v === undefined) {
        return MalType.nil;
      } else {
        switch (v.tag) {
          case "MalFunction":
          case "MalHashMap":
          case "MalInternalFunction":
          case "MalList":
          case "MalVector":
            return v.meta ?? MalType.nil;
          default:
            return MalType.nil;
        }
      }
    }),
  ],
  [
    "with-meta",
    MalType.mkInternalFunction(([v, m]) => {
      if (v === undefined || m === undefined) {
        throw MalType.mkList(
          [
            MalType.mkSymbol("IncorrectParameter"),
            MalType.mkString("with-meta"),
          ],
        );
      }
      return MalType.withMeta(v, m);
    }),
  ],
  [
    "fn?",
    MalType.mkInternalFunction(([v]) =>
      MalType.mkBoolean(
        v !== undefined &&
          (v.tag === "MalFunction" && !v.isMacro ||
            v.tag === "MalInternalFunction"),
      )
    ),
  ],
  [
    "macro?",
    MalType.mkInternalFunction(([v]) =>
      MalType.mkBoolean(v !== undefined && v.tag === "MalFunction" && v.isMacro)
    ),
  ],
  [
    "seq",
    MalType.mkInternalFunction(([s]) => {
      if (s === undefined) {
        throw MalType.mkList(
          [
            MalType.mkSymbol("IncorrectParameter"),
            MalType.mkString("seq"),
          ],
        );
      }

      if (s.tag === "MalList") {
        return s.items.length === 0 ? MalType.nil : s;
      } else if (s.tag === "MalVector") {
        return s.items.length === 0 ? MalType.nil : MalType.mkList(s.items);
      } else if (s.tag === "MalNil") {
        return s;
      } else if (s.tag === "MalString") {
        return s.value.length === 0
          ? MalType.nil
          : MalType.mkList(s.value.split("").map((e) => MalType.mkString(e)));
      } else {
        throw MalType.mkList(
          [
            MalType.mkSymbol("IncorrectParameter"),
            MalType.mkString("seq"),
          ],
        );
      }
    }),
  ],
  [
    "conj",
    MalType.mkInternalFunction(([c, ...es]) => {
      if (c === undefined) {
        throw MalType.mkList(
          [
            MalType.mkSymbol("IncorrectParameter"),
            MalType.mkString("conj"),
          ],
        );
      }
      if (es === undefined) {
        return c;
      } else if (c.tag === "MalList") {
        return MalType.mkList([...es.reverse(), ...c.items]);
      } else if (c.tag === "MalVector") {
        return MalType.mkVector([...c.items, ...es]);
      } else {
        throw MalType.mkList(
          [
            MalType.mkSymbol("IncorrectParameterType"),
            MalType.mkString("conj"),
            MalType.mkList(
              [MalType.mkSymbol("list"), MalType.mkSymbol("vector")],
            ),
            MalType.mkString(c.tag),
          ],
        );
      }
    }),
  ],
];

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
