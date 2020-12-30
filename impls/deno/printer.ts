import { MalType } from "./types.ts";

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
      case "MalMap":
        return `{${
          [...v.items].map(([k, v]) =>
            `${
              k.startsWith("s")
                ? prStrReadably({ tag: "MalString", value: k.substr(1) })
                : prStrReadably({ tag: "MalSymbol", name: k.substr(1) })
            } ${prStrReadably(v)}`
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
      case "MalAtom":
        return `${v.value}`;
      case "MalSymbol":
        return `${v.name}`;
    }
  };

  return prStrReadably(v);
};
