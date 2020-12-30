import { MalType, mapValues } from "./types.ts";

export const prStr = (
  v: MalType,
  printReabably: boolean = true,
): string => {
  const prStrReadably = (v: MalType): string => {
    switch (v.tag) {
      case "MalList":
        return `(${v.items.map(prStrReadably).join(" ")})`;
      case "MalVector":
        return `[${v.items.map(prStrReadably).join(" ")}]`;
      case "MalHashMap":
        return `{${
          mapValues(v).map(([k, v]) =>
            `${prStrReadably(k)} ${prStrReadably(v)}`
          ).join(" ")
        }}`;
      case "MalNil":
        return "nil";
      case "MalString":
        return printReabably
          ? `"${
            v.value.replaceAll("\\", "\\\\").replaceAll('"', '\\"').replaceAll(
              "\n",
              "\\n",
            )
          }"`
          : v.value;
      case "MalBoolean":
      case "MalNumber":
        return `${v.value}`;
      case "MalKeyword":
      case "MalSymbol":
        return `${v.name}`;
      case "MalFunction":
        return "#<function>";
    }
  };

  return prStrReadably(v);
};
