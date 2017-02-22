import { readline } from "./node_readline";

function read(v: string): any {
    // TODO
    return v;
}

function evalAST(v: any): any {
    // TODO
    return v;
}

function print(v: any): string {
    // TODO
    return v;
}

function rep(v: string): string {
    // TODO
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
    console.log(rep(line));
}
