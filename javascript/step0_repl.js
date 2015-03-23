function READ(x) {
    return x;
}

function EVAL(x) {
    return x;
}

function PRINT(x) {
    return x;
}

function rep(x) {
    return PRINT(EVAL(READ(x)));
}

var readline = require("readline"),
    rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });

rl.setPrompt("user> ");
rl.prompt();

rl.on("line", function(line) {
    console.log(rep(line));
    rl.prompt();
}).on("exit", function() {
    rl.close();
    process.exit(0);
});
