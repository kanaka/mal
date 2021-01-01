import * as Env from "./env.ts";

export type MalType =
  | MalAtom
  | MalBoolean
  | MalFunction
  | MalHashMap
  | MalInternalFunction
  | MalKeyword
  | MalList
  | MalNil
  | MalNumber
  | MalString
  | MalSymbol
  | MalVector;

export type MalAtom = {
  tag: "MalAtom";
  value: MalType;
};

export const mkAtom = (value: MalType): MalAtom => ({ tag: "MalAtom", value });

export type MalList = {
  tag: "MalList";
  items: Array<MalType>;
};

export const mkList = (items: Array<MalType>): MalList => ({
  tag: "MalList",
  items,
});

export type MalVector = {
  tag: "MalVector";
  items: Array<MalType>;
};

export const mkVector = (items: Array<MalType>): MalVector => ({
  tag: "MalVector",
  items,
});

export type MalHashMap = {
  tag: "MalHashMap";
  items: Map<string, MalType>;
};

export const mkHashMap = (values: Array<[MalType, MalType]>): MalHashMap => {
  const items: Array<[string, MalType]> = [];

  values.forEach(([k, v]) => {
    if (k.tag === "MalString") {
      items.push([`s${k.value}`, v]);
    } else if (k.tag === "MalKeyword") {
      items.push([`t${k.name}`, v]);
    } else {
      throw new Error(
        `Precondition Error: Unable to use ${
          JSON.stringify(k)
        } as a hashmap key.`,
      );
    }
  });

  return { tag: "MalHashMap", items: new Map(items) };
};

export const mapValues = (malMap: MalHashMap): Array<[MalType, MalType]> =>
  [...malMap.items].map(([k, v]) =>
    k.startsWith("s") ? [mkString(k.substr(1)), v] : [mkKeyword(k.substr(1)), v]
  );

export type MalNumber = {
  tag: "MalNumber";
  value: number;
};

export const mkNumber = (value: number): MalNumber => ({
  tag: "MalNumber",
  value,
});

export const asNumber = (v: MalType): number => {
  if (v.tag === "MalNumber") {
    return v.value;
  } else {
    throw new Error(`Precondition Error: ${JSON.stringify(v)} is not a number`);
  }
};

export type MalString = {
  tag: "MalString";
  value: string;
};

export const mkString = (value: string): MalString => ({
  tag: "MalString",
  value,
});

export type MalBoolean = {
  tag: "MalBoolean";
  value: boolean;
};

export const mkBoolean = (value: boolean): MalBoolean => ({
  tag: "MalBoolean",
  value,
});

export type MalNil = {
  tag: "MalNil";
};

export const nil: MalNil = ({ tag: "MalNil" });

export type MalSymbol = {
  tag: "MalSymbol";
  name: string;
};

export const mkSymbol = (name: string): MalSymbol => ({
  tag: "MalSymbol",
  name,
});

export type MalKeyword = {
  tag: "MalKeyword";
  name: string;
};

export const mkKeyword = (name: string): MalKeyword => ({
  tag: "MalKeyword",
  name,
});

export type MalInternalFunction = {
  tag: "MalInternalFunction";
  fn: (args: Array<MalType>) => MalType;
};

export const mkInternalFunction = (
  fn: (args: Array<MalType>) => MalType,
): MalInternalFunction => ({ tag: "MalInternalFunction", fn });

export type MalFunction = {
  tag: "MalFunction";
  body: MalType;
  params: Array<MalSymbol>;
  env: Env.Env;
};

export const mkFunction = (
  body: MalType,
  params: Array<MalSymbol>,
  env: Env.Env,
): MalFunction => ({
  tag: "MalFunction",
  body,
  params,
  env,
});

export const equals = (a: MalType, b: MalType): boolean => {
  switch (a.tag) {
    case "MalBoolean":
      return b.tag === "MalBoolean" && a.value === b.value;
    case "MalHashMap":
      return b.tag === "MalHashMap" && hashMapEquals(a, b);
    case "MalKeyword":
      return b.tag === "MalKeyword" && a.name === b.name;
    case "MalList":
    case "MalVector":
      return (b.tag === "MalList" || b.tag === "MalVector") && seqEquals(a, b);
    case "MalNil":
      return b.tag === "MalNil";
    case "MalNumber":
      return b.tag === "MalNumber" && a.value === b.value;
    case "MalString":
      return b.tag === "MalString" && a.value === b.value;
    case "MalSymbol":
      return b.tag === "MalSymbol" && a.name === b.name;
    default:
      return false;
  }
};

const hashMapEquals = (a: MalHashMap, b: MalHashMap): boolean => {
  const as = a.items;
  const bs = b.items;

  if (as.size !== bs.size) {
    return false;
  }

  as.forEach((value, key) => {
    if (!bs.has(key)) return false;

    const bValue = bs.get(key);

    if (bValue === undefined || !equals(value, bValue)) {
      return false;
    }
  });

  return true;
};

const seqEquals = (
  a: MalList | MalVector,
  b: MalList | MalVector,
): boolean => {
  const as = a.items;
  const bs = b.items;

  if (as.length !== bs.length) {
    return false;
  }

  for (let loop = 0; loop < as.length; loop += 1) {
    if (!equals(as[loop], bs[loop])) {
      return false;
    }
  }

  return true;
};
