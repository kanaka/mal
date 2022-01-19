import promptImp = require('prompt-sync')
const prompt = promptImp({sigint: true})

function READ(input: string): string {
    return input
}

function EVAL(input: string): string {
    return input
}

function PRINT(input: string): string {
    return input
}

function REP(input: string): string {
    const r = READ(input)
    const e = EVAL(r)
    const p = PRINT(e)
    return p
}

while (true) {

    let line: string = prompt('user> ')

    if (line == null) {
        break;
    }
    if (line === "") {
        continue;
    }

    console.log(REP(line))
}