#ifndef __MAL_READER__
#define __MAL_READER__

#include <glib.h>
#include <glib-object.h>

#include "types.h"

typedef struct {
    GArray *array;
    int position;
} Reader;

Reader *reader_new();
int reader_append(Reader *reader, char* token);
char *reader_peek(Reader *reader);
char *reader_next(Reader *reader);
void reader_free(Reader *reader);

char *_readline (char prompt[]);
MalVal *read_str ();

#endif
