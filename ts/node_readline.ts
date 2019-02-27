import * as path from "path";
import * as ffi from "ffi-napi";
import * as fs from "fs";

// IMPORTANT: choose one
const RL_LIB = "libreadline";  // NOTE: libreadline is GPL
// var RL_LIB = "libedit";

const HISTORY_FILE = path.join(process.env.HOME || ".", ".mal-history");

const rllib = ffi.Library(RL_LIB, {
    "readline": ["string", ["string"]],
    "add_history": ["int", ["string"]],
});

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
            if (lines[i]) { rllib.add_history(lines[i]); }
        }
    }

    const line = rllib.readline(prompt);
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
