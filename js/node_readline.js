// IMPORTANT: choose one
var RL_LIB = "libreadline";  // NOTE: libreadline is GPL
//var RL_LIB = "libedit";

var HISTORY_FILE = require('path').join(process.env.HOME, '.mal-history');

var rlwrap = {}; // namespace for this module in web context

var ffi = require('ffi'),
    fs = require('fs');

var rllib = ffi.Library(RL_LIB, {
    'readline':    [ 'string', [ 'string' ] ],
    'add_history': [ 'int',    [ 'string' ] ]});

var rl_history_loaded = false;

exports.readline = rlwrap.readline = function(prompt) {
    prompt = prompt || "user> ";

    if (!rl_history_loaded) {
        rl_history_loaded = true;
        var lines = [];
        if (fs.existsSync(HISTORY_FILE)) {
            lines = fs.readFileSync(HISTORY_FILE).toString().split("\n");
        }
        // Max of 2000 lines
        lines = lines.slice(Math.max(lines.length - 2000, 0));
        for (var i=0; i<lines.length; i++) {
            if (lines[i]) { rllib.add_history(lines[i]); }
        }
    }

    var line = rllib.readline(prompt);
    if (line) {
        rllib.add_history(line);
        fs.appendFileSync(HISTORY_FILE, line + "\n");
    }

    return line;
};
var readline = exports;
