// IMPORTANT: choose one
var RL_LIB = "libreadline.so";  // NOTE: libreadline is GPL
//var RL_LIB = "libedit.so";

var HISTORY_FILE = require('path').join(process.env.HOME, '.mal-history');

var rlwrap = {}; // namespace for this module in web context

var koffi = require('koffi'),
    fs = require('fs');

var koffi_rl = koffi.load(RL_LIB)

var rllib = {
    readline: koffi_rl.func("char *readline(char *prompt)"),
    add_history: koffi_rl.func("int add_history(char *line)")
}

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
            if (lines[i]) { rllib.add_history(lines[i]); }
        }
    }

    var line = rllib.readline(prompt);
    if (line) {
        rllib.add_history(line);
        try {
            fs.appendFileSync(HISTORY_FILE, line + "\n");
        } catch (exc) {
            // ignored
        }
    }

    return line;
};
var readline = exports;

