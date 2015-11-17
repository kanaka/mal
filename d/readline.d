import std.string;
import std.path;

// readline/readline.h
extern (C) char* readline(const char* prompt);

// readline/history.h
extern (C) void using_history();
extern (C) void add_history(const char *line);
extern (C) int read_history(const char *filename);
extern (C) int append_history(int nelement, const char *filename);

bool history_loaded = false;
const string history_file = "~/.mal-history";

void load_history()
{
    if (history_loaded) return;
    using_history();
    string hf = expandTilde(history_file);
    std.file.append(hf, ""); // Create the file if needed
    read_history(toStringz(hf));
    history_loaded = true;
}

void append_to_history()
{
    string hf = expandTilde(history_file);
    append_history(1, toStringz(hf));
}

// Convert from C-string to D-string (making a copy)
pure string fromCstr(char* cstr)
{
    auto len = core.stdc.string.strlen(cstr);
    if (len == 0) return "";
    string line = cstr[0..len].dup;
    return line;
}

string _readline(in string prompt)
{
    load_history();

    auto cstr = readline(toStringz(prompt));
    if (cstr is null) return null;
    scope(exit) { core.stdc.stdlib.free(cstr); }

    if (cstr[0] != '\0')
    {
        add_history(cstr);   // Add input to in-memory history
        append_to_history(); // Flush new line of history to disk
    }

    return fromCstr(cstr);
}
