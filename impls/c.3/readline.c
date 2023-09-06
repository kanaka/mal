#include <readline/readline.h>
#include <readline/history.h>
#include <stdlib.h>

#include "readline.h"

char *_readline(char *prompt)
{
    return readline(prompt);
}

void _save_history(char *file_name)
{
    HISTORY_STATE *history_state = history_get_history_state();
    HIST_ENTRY **history = history_list();
    FILE *stream;

    if (!history)
    {
        return;
    }

    stream = fopen(file_name, "w");

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

void _read_history(char *file_name)
{
    FILE *stream = fopen(file_name, "r");
    char *buffer = (char *)NULL;
    char *line = (char *)NULL;

    int len = 0;
    size_t bytes_read;

    if (stream)
    {
        fseek(stream, 0, SEEK_END);
        len = ftell(stream);
        fseek(stream, 0, SEEK_SET);

        buffer = malloc(len + 1);
        bytes_read = fread(buffer, 1, len, stream);
        fclose(stream);

        if (bytes_read != len)
        {
            printf("Read %lu bytes, expected %d.", bytes_read, len);
        }

        int i = 0;

        while (i < len)
        {
            line = buffer + i;

            for (int j = i; j < len; j++)
            {
                if (buffer[j] == '\n')
                {
                    buffer[j] = '\0';
                    add_history(line);
                    i = j + 1;
                    break;
                }
            }
        }
    }
}