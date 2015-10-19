#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#if USE_READLINE
  #include <readline/readline.h>
  #include <readline/history.h>
  #include <readline/tilde.h>
#else
  #include <editline/readline.h>
#endif

int history_loaded = 0;

char HISTORY_FILE[] = "~/.mal-history";

void load_history() {
    if (history_loaded) { return; }
    int ret;
    char *hf = tilde_expand(HISTORY_FILE);
    if (access(hf, F_OK) != -1) {
        // TODO: check if file exists first, use non-static path
#if USE_READLINE
        ret = read_history(hf);
#else
        FILE *fp = fopen(hf, "r");
        char *line = malloc(80); // getline reallocs as necessary
        size_t sz = 80;
        while ((ret = getline(&line, &sz, fp)) > 0) {
            add_history(line); // Add line to in-memory history
        }
        free(line);
        fclose(fp);
#endif
        history_loaded = 1;
    }
    free(hf);
}

void append_to_history() {
    char *hf = tilde_expand(HISTORY_FILE);
#ifdef USE_READLINE
    append_history(1, hf);
#else
#if defined(RL_READLINE_VERSION)
    HIST_ENTRY *he = history_get(history_base+history_length-1);
#else
    // libedit-2 segfaults if we add history_base
    HIST_ENTRY *he = history_get(history_length-1);
#endif
    FILE *fp = fopen(hf, "a");
    if (fp) {
        fprintf(fp, "%s\n", he->line);
        fclose(fp);
    }
#endif
    free(hf);
}


// line must be freed by caller
char *_readline (char prompt[]) {
    char *line;

    load_history();

    line = readline(prompt);
    if (!line) return NULL; // EOF
    add_history(line); // Add input to in-memory history

    append_to_history(); // Flush new line of history to disk

    return line;
}

