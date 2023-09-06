#ifndef _MAL_READLINE_H
#define _MAL_READLINE_H
char *_readline(char *input);
void _read_history(char *file_name);
void _save_history(char *file_name);
void _add_history(char *);
#endif