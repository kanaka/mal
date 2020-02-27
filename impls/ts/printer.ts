import { Node, MalType } from "./types";

export function prStr(v: MalType, printReadably = true): string {
    switch (v.type) {
        case Node.List:
            return `(${v.list.map(v => prStr(v, printReadably)).join(" ")})`;
        case Node.Vector:
            return `[${v.list.map(v => prStr(v, printReadably)).join(" ")}]`;
        case Node.HashMap:
            let result = "{";
            for (const [key, value] of v.entries()) {
                if (result !== "{") {
                    result += " ";
                }
                result += `${prStr(key, printReadably)} ${prStr(value, printReadably)}`;
            }
            result += "}";
            return result;
        case Node.Number:
        case Node.Symbol:
        case Node.Boolean:
            return `${v.v}`;
        case Node.String:
            if (printReadably) {
                const str = v.v
                    .replace(/\\/g, "\\\\")
                    .replace(/"/g, '\\"')
                    .replace(/\n/g, "\\n");
                return `"${str}"`;
            } else {
                return v.v;
            }
        case Node.Nil:
            return "nil";
        case Node.Keyword:
            return `:${v.v}`;
        case Node.Function:
            return "#<function>";
        case Node.Atom:
            return `(atom ${prStr(v.v, printReadably)})`;
    }
}
