export type MalType =
  | MalList
  | MalVector
  | MalHashMap
  | MalNumber
  | MalString
  | MalBoolean
  | MalNil
  | MalSymbol
  | MalKeyword
  | MalFunction;

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

export type MalFunction = {
  tag: "MalFunction";
  f: (args: Array<MalType>) => MalType;
};

export const mkFunction = (
  f: (args: Array<MalType>) => MalType,
): MalFunction => ({ tag: "MalFunction", f });
