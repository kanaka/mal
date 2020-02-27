import { readline } from "./node_readline";

import { MalType } from "./types";
import { readStr } from "./reader";
import { prStr } from "./printer";

// READ
function read(str: string): MalType {
    return readStr(str);
}

// EVAL
function evalMal(ast: any, _env?: any): any {
    // TODO
    return ast;
}

// PRINT
function print(exp: MalType): string {
    return prStr(exp);
}

function rep(str: string): string {
    return print(evalMal(read(str)));
}

while (true) {
    const line = readline("user> ");
    if (line == null) {
        break;
    }
    if (line === "") {
        continue;
    }
    try {
        console.log(rep(line));
    } catch (e) {
        const err: Error = e;
        console.error(err.message);
    }
}
