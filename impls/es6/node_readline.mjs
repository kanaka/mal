// IMPORTANT: choose one
const RL_LIB = "libreadline.so.8";  // NOTE: libreadline is GPL
//const RL_LIB = "libedit.so.2";

import path from 'path';
import fs from 'fs';
const koffiCjs = await import('koffi');
const koffi = koffiCjs.default || koffiCjs;

const HISTORY_FILE = path.join(process.env.HOME, '.mal-history');
const rllib = koffi.load(RL_LIB);
const readlineFunc = rllib.func('char *readline(char *)');
const addHistoryFunc = rllib.func('int add_history(char *)');

var rl_history_loaded = false;

function readline(prompt) {
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

export default { readline };
