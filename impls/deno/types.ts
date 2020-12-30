export type MalType =
  | MalList
  | MalVector
  | MalHashMap
  | MalAtom
  | MalNumber
  | MalString
  | MalBoolean
  | MalNil
  | MalSymbol;

export type MalList = {
  tag: "MalList";
  items: Array<MalType>;
};

export type MalVector = {
  tag: "MalVector";
  items: Array<MalType>;
};

export type MalHashMap = {
  tag: "MalHashMap";
  items: Map<string, MalType>;
};

export const mkMalMap = (values: Array<[MalType, MalType]>): MalHashMap => {
  const items: Array<[string, MalType]> = [];

  values.forEach(([k, v]) => {
    if (k.tag === "MalString") {
      items.push([`s${k.value}`, v]);
    } else if (k.tag === "MalSymbol") {
      items.push([`t${k.name}`, v]);
    } else {
      throw new Error(
        `Precondition Error: Unable to use ${JSON.stringify(k)} as a hashmap key.`,
      );
    }
  });

  return { tag: "MalHashMap", items: new Map(items) };
};

export const mapValues = (malMap: MalHashMap): Array<[MalType, MalType]> =>
  [...malMap.items].map(([k, v]) =>
    k.startsWith("s")
      ? [{ tag: "MalString", value: k.substr(1) }, v]
      : [{ tag: "MalSymbol", name: k.substr(1) }, v]
  );

export type MalAtom = {
  tag: "MalAtom";
  value: string;
};

export type MalNumber = {
  tag: "MalNumber";
  value: number;
};

export type MalString = {
  tag: "MalString";
  value: string;
};

export type MalBoolean = {
  tag: "MalBoolean";
  value: boolean;
};

export type MalNil = {
  tag: "MalNil";
};

export type MalSymbol = {
  tag: "MalSymbol";
  name: string;
};
