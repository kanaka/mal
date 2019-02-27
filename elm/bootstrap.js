var readline = require('./node_readline');
var fs = require('fs');

// The first two arguments are: 'node' and 'bootstrap.js'
// The third argument is the name of the Elm module to load.
var args = process.argv.slice(2);
var mod = require('./' + args[0]);

var app = mod.Main.worker({
    args: args.slice(1)
});

// Hook up the writeLine and readLine ports of the app.
app.ports.writeLine.subscribe(function(line) {
    console.log(line);
    app.ports.input.send({"tag": "lineWritten"});
});

app.ports.readLine.subscribe(function(prompt) {
    var line = readline.readline(prompt);
    app.ports.input.send({"tag": "lineRead", "line": line});
});

// Read the contents of a file.
app.ports.readFile.subscribe(function(filename) {
    try {
        var contents = fs.readFileSync(filename, 'utf8');
        app.ports.input.send({"tag": "fileRead", "contents": contents});
    } catch (e) {
        app.ports.input.send({"tag": "exception", "message": e.message});
    }
});