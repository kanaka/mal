import * as path from "path";
import * as koffi from "koffi";
import * as fs from "fs";

// IMPORTANT: choose one
const RL_LIB = "libreadline.so.8";  // NOTE: libreadline is GPL
// const RL_LIB = "libedit.so.2";

const HISTORY_FILE = path.join(process.env.HOME || ".", ".mal-history");

let rllib: any;
try {
    rllib = koffi.load(RL_LIB);
} catch (e) {
    console.error('ERROR loading RL_LIB:', RL_LIB, e);
    throw e;
}

const readlineFunc = rllib.func('char *readline(char *)');
const addHistoryFunc = rllib.func('int add_history(char *)');

let rlHistoryLoaded = false;

export function readline(prompt?: string): string | null {
    prompt = prompt || "user> ";

    if (!rlHistoryLoaded) {
        rlHistoryLoaded = true;
        let lines: string[] = [];
        if (fs.existsSync(HISTORY_FILE)) {
            lines = fs.readFileSync(HISTORY_FILE).toString().split("\n");
        }
        // Max of 2000 lines
        lines = lines.slice(Math.max(lines.length - 2000, 0));
        for (let i = 0; i < lines.length; i++) {
            if (lines[i]) { addHistoryFunc(lines[i]); }
        }
    }

    const line = readlineFunc(prompt);
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
