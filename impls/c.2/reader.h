#ifndef _MAL_READER_H
#define _MAL_READER_H

#include "types.h"

typedef struct Token_s {

  int type;
  char* data;
  char* error;

} Token;

typedef struct Reader_s {

  long position;      // current position in the array
  long token_count;   // number of tokens in the array
  long max_tokens;    // maximum number of tokens the array can hold
  Token** token_data; // pointer to an array of Tokens
  char* error;        // error message

} Reader;

/* reader object */
Reader* reader_make(long token_capacity);
Reader* reader_append(Reader* reader, Token* token);
Token* reader_peek(const Reader* reader);
Token* reader_next(Reader* reader);
Token* reader_get_at(const Reader* reader, long i);
void reader_print(Reader* reader);

/* tokenizing the input */
Reader* tokenize(char* token_string);
char* read_fixed_length_token(char* current, Token** ptoken, int n);
char* read_string_token(char* current, Token** ptoken);
char* read_comment_token(char* current, Token** ptoken);
//char* read_integer_token(char* current, Token** ptoken);
char* read_number_token(char* current, Token** ptoken);
char* read_symbol_token(char* current, Token** ptoken);
char* read_keyword_token(char* current, Token** ptoken);

/* reading the tokens into types */
MalType* read_str(char* token_string);
MalType* read_form(Reader* reader);
MalType* read_atom(Reader* reader);
MalType* read_list(Reader* reader);
MalType* read_vector(Reader* reader);
MalType* read_hashmap(Reader* reader);

/* utility functions */
char* read_terminated_token (char* current, Token** ptoken, int type);
MalType* read_matched_delimiters(Reader* reader, char start_delimiter, char end_delimiter);
MalType* make_symbol_list(Reader* reader, char* symbol_name);
Token* token_allocate(char* str, long num_chars, int type, char* error);
char* unescape_string(char* str, long length);

#endif
