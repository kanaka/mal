import { readline } from "./node_readline";

// READ
function read(str: string): any {
    // TODO
    return str;
}

// EVAL
function evalMal(ast: any, _env?: any): any {
    // TODO
    return ast;
}

// PRINT
function print(exp: any): string {
    // TODO
    return exp;
}

function rep(str: string): string {
    // TODO
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
    console.log(rep(line));
}
