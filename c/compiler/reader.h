#ifndef __MAL_READER__
#define __MAL_READER__

#include <stddef.h>
#include "types.h"

typedef struct Token {
  char* str;
  struct Token* next;
} Token;

typedef struct Reader {
  Token* token;
} Reader;

char* reader_next(Reader* reader);

char* reader_peek(Reader* reader);

static const char PATTERN[] = "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}('\"`,;)]*)";

Token* tokenize(char* code);

MalType* read_str(char* code);
MalType* read_form(Reader* reader);
MalType* read_keyword(Reader *reader);
MalType* read_quote(Reader *reader, const char *expanded);
MalType* read_with_meta(Reader *reader);
MalType* read_list(Reader* reader);
MalType* read_vector(Reader* reader);
MalType* read_hashmap(Reader *reader);
MalType* read_string(Reader *reader);
char unescape_char(char *token, size_t *i, size_t len);
MalType* read_atom(Reader* reader);

#endif
