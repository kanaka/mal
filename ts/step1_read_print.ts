import { readline } from "./node_readline";

import { MalType } from "./types";
import { readStr } from "./reader";
import { prStr } from "./printer";

function read(v: string): MalType {
    return readStr(v);
}

function evalAST(v: any): any {
    // TODO
    return v;
}

function print(v: MalType): string {
    return prStr(v);
}

function rep(v: string): string {
    return print(evalAST(read(v)));
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
