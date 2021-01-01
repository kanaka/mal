import * as Env from "./env.ts";
import * as MalType from "./types.ts";
import * as Printer from "./printer.ts";
import * as Reader from "./reader.ts";

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
];
