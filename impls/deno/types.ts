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

export const mkHashMap = (values: Array<[MalType, MalType]>): MalHashMap => ({
  tag: "MalHashMap",
  items: new Map(values.map(([k, v]) => [mapHashMapKey(k), v])),
});

export const mapAssoc = (
  malMap: MalHashMap,
  values: Array<[MalType, MalType]>,
): MalHashMap => {
  const result = new Map(malMap.items);

  values.forEach(([k, v]) => {
    result.set(mapHashMapKey(k), v);
  });

  return { tag: "MalHashMap", items: result };
};

export const mapDissoc = (
  malMap: MalHashMap,
  values: Array<MalType>,
): MalHashMap => {
  const result = new Map(malMap.items);

  values.forEach((k) => {
    result.delete(mapHashMapKey(k));
  });

  return { tag: "MalHashMap", items: result };
};

export const mapGet = (malMap: MalHashMap, key: MalType): MalType =>
  malMap.items.get(mapHashMapKey(key)) ?? nil;

export const mapContains = (malMap: MalHashMap, key: MalType): MalType =>
  mkBoolean(malMap.items.get(mapHashMapKey(key)) !== undefined);

export const mapKeys = (malMap: MalHashMap): Array<MalType> =>
  [...malMap.items].map(([k, _]) => reverseMapHashMapKey(k));

export const mapValues = (malMap: MalHashMap): Array<MalType> =>
  [...malMap.items].map(([_, v]) => v);

export const mapKeyValues = (malMap: MalHashMap): Array<[MalType, MalType]> =>
  [...malMap.items].map(([k, v]) => [reverseMapHashMapKey(k), v]);

const mapHashMapKey = (k: MalType): string => {
  if (k.tag === "MalString") {
    return `s${k.value}`;
  } else if (k.tag === "MalKeyword") {
    return `t${k.name}`;
  } else {
    throw new Error(
      `Precondition Error: Unable to use ${
        JSON.stringify(k)
      } as a hashmap key.`,
    );
  }
};

const reverseMapHashMapKey = (k: string): MalString | MalKeyword =>
  k.startsWith("s") ? mkString(k.substr(1)) : mkKeyword(k.substr(1));

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

const booleanTrue: MalBoolean = { tag: "MalBoolean", value: true };

const booleanFalse: MalBoolean = { tag: "MalBoolean", value: false };

export const mkBoolean = (value: boolean): MalBoolean =>
  value ? booleanTrue : booleanFalse;

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
  isMacro: boolean;
};

export const mkFunction = (
  body: MalType,
  params: Array<MalSymbol>,
  env: Env.Env,
  isMacro: boolean = false,
): MalFunction => ({
  tag: "MalFunction",
  body,
  params,
  env,
  isMacro,
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

  for (let [key, value] of as) {
    if (!bs.has(key)) return false;

    const bValue = bs.get(key);

    if (bValue === undefined || !equals(value, bValue)) {
      return false;
    }
  }

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
