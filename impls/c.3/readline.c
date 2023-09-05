#include <readline/readline.h>
#include <readline/history.h>
#include <stdlib.h>

#include "readline.h"

char *_readline(char *prompt)
{
    return readline(prompt);
}

void _save_history()
{
    HISTORY_STATE *history_state = history_get_history_state();
    HIST_ENTRY **history = history_list();
    FILE *stream;

    if (!history)
    {
        return;
    }

// FIXME: Use getenv to get at home folder
    stream = fopen("/home/weigo/.mal_history", "w+");

    for (int i = 0; i < history_state->length; i++)
    {
        fprintf(stream, "%s\n", history[i]->line);
        free(history[i]);
    }

    fclose(stream);

    free(history_state);
    free(history);
}

void _add_history(char *line)
{
    add_history(line);
}