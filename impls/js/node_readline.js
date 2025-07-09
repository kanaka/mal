// IMPORTANT: choose one
var RL_LIB = "libreadline.so.8";  // NOTE: libreadline is GPL
//var RL_LIB = "libedit.so.2";

var HISTORY_FILE = require('path').join(process.env.HOME, '.mal-history');

var rlwrap = {}; // namespace for this module in web context

const koffi = require('koffi');
const fs = require('fs');

let rllib;
try {
    rllib = koffi.load(RL_LIB);
} catch (e) {
    console.error('ERROR loading RL_LIB:', RL_LIB, e);
    throw e;
}
const readlineFunc = rllib.func('char *readline(char *)');
const addHistoryFunc = rllib.func('int add_history(char *)');

var rl_history_loaded = false;

exports.readline = rlwrap.readline = function(prompt) {
    prompt = typeof prompt !== 'undefined' ? prompt : "user> ";

    if (!rl_history_loaded) {
        rl_history_loaded = true;
        var lines = [];
        if (fs.existsSync(HISTORY_FILE)) {
            lines = fs.readFileSync(HISTORY_FILE).toString().split("\n");
        }
        // Max of 2000 lines
        lines = lines.slice(Math.max(lines.length - 2000, 0));
        for (var i=0; i<lines.length; i++) {
            if (lines[i]) { addHistoryFunc(lines[i]); }
        }
    }

    var line = readlineFunc(prompt);
    if (line) {
        addHistoryFunc(line);
        try {
            fs.appendFileSync(HISTORY_FILE, line + "\n");
        } catch (exc) {
            // ignored
        }
    }

    return line;
};
//module.exports = { readline };
var readline = exports;
