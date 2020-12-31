import * as MalType from "./types.ts";
import * as Printer from "./printer.ts";

export const ns: Array<[string, MalType.MalType]> = [
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
    MalType.mkInternalFunction(([a, b]) => MalType.mkBoolean(MalType.equals(a, b))),
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
];
