#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <gc.h>

#include "reader.h"

#define TOKEN_SPECIAL_CHARACTER 1
#define TOKEN_STRING 2
#define TOKEN_INTEGER 3
#define TOKEN_FLOAT 4
#define TOKEN_SYMBOL 5
#define TOKEN_COMMENT 6
#define TOKEN_KEYWORD 7
#define TOKEN_TRUE 8
#define TOKEN_FALSE 9
#define TOKEN_NIL 10

#define SYMBOL_NIL "nil"
#define SYMBOL_TRUE "true"
#define SYMBOL_FALSE "false"
#define SYMBOL_QUOTE "quote"
#define SYMBOL_QUASIQUOTE "quasiquote"
#define SYMBOL_UNQUOTE  "unquote"
#define SYMBOL_SPLICE_UNQUOTE "splice-unquote"
#define SYMBOL_DEREF "deref"
#define SYMBOL_WITH_META "with-meta"

Reader* reader_make(long token_capacity) {

  Reader* reader = GC_MALLOC(sizeof(*reader));

  reader->max_tokens = token_capacity;
  reader->position = 0;
  reader->token_count = 0;
  reader->token_data = GC_MALLOC(sizeof(Token*) * token_capacity);
  reader->error = NULL;

  return reader;
}

Reader* reader_append(Reader* reader, Token* token) {

  if (reader->token_count < reader->max_tokens) {

    reader->token_data[reader->token_count] = token;
    reader->token_count++;
  }
  else {
    /* TODO: expand the storage more intelligently */
    reader->max_tokens *= 2;
    reader = GC_REALLOC(reader, sizeof(*reader) * reader->max_tokens);
    reader->token_data[reader->token_count] = token;
    reader->token_count++;
  }
  return reader;
}

Token* reader_peek(const Reader* reader) {

  return (reader->token_data[reader->position]);
}

Token* reader_next(Reader* reader) {

  Token* tok = reader->token_data[reader->position];

  if (reader->position == -1) {
    return NULL;
  }
  else if (reader->position < reader->token_count) {
    (reader->position)++;
    return tok;
  }
  else {
    reader->position = -1;
    return tok;
  }
}

void reader_print(Reader* reader) {
  /* NOTE: needed for debugging the reader only */

  Token* tok;

  for (long i = 0; i < reader->token_count; i++) {

    tok =  reader_next(reader);

    switch (tok->type) {
    case TOKEN_SPECIAL_CHARACTER:
      printf("special character: %s", tok->data);
      break;
    case TOKEN_STRING:
      printf("string: %s", tok->data);
      break;
    case TOKEN_INTEGER:
      printf("integer: %s", tok->data);
      break;
    case TOKEN_FLOAT:
      printf("float: %s", tok->data);
      break;
    case TOKEN_SYMBOL:
      printf("symbol: %s", tok->data);
      break;
    case TOKEN_COMMENT:
      printf("comment: \"%s\"", tok->data);
      break;
    case TOKEN_KEYWORD:
      printf("keyword: %s", tok->data);
      break;
    case TOKEN_TRUE:
      printf("true: %s", tok->data);
      break;
    case TOKEN_FALSE:
      printf("false: %s", tok->data);
      break;
    case TOKEN_NIL:
      printf("nil: %s", tok->data);
      break;
    }
    /* print an error for any tokens with an error string */
    tok->error ? printf(" - %s", tok->error) : 0;
  }
}

MalType* read_str(char* token_string) {

  Reader* reader = tokenize(token_string);

  if (reader->error) {
    return make_error_fmt("Reader error: %s", reader->error);
  }
  else if (reader->token_count == 0) {
    return make_nil();
  }
  else {
    return read_form(reader);
  }
}

Reader* tokenize(char* token_string) {

  /* allocate enough space for a Reader */
  /* TODO: over-allocates space */
  Reader* reader = reader_make(strlen(token_string));

  for (char* next = token_string; *next != '\0';) {

    Token* token = NULL;

    switch (*next) {
      /* skip whitespace */
    case ' ':
    case ',':
    case 0x0A: /* newline */
      next++;
      token = NULL; /* no token for whitespace */
      break;

      /* single character token */
    case '[':
    case '\\':
    case ']':
    case '{':
    case '}':
    case '(':
    case ')':
    case '\'':
    case '@':
    case '`':
    case '^':
      next = read_fixed_length_token(next, &token, 1);
      break;

      /* single or double character token */
    case '~':
      if ( *(next + 1) == '@' ) {
        next = read_fixed_length_token(next, &token, 2);
      }
      else {
        next = read_fixed_length_token(next, &token, 1);
      }
      break;

      /* read string of characters within double quotes */
    case '"':
      next = read_string_token(next, &token);
      break;

      /* read a comment - all remaining input until newline */
    case ';':
      next = read_comment_token(next, &token);
      token = NULL; /* skip token for comments */
      break;

      /* read an integer */
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      next = read_number_token(next, &token);
      //      next = read_integer_token(next, &token);
      break;

      /* integer may be prefixed with +/- */
    case '+':
    case '-':
      if (isdigit(next[1])) {
        next = read_number_token(next, &token);
        //      next = read_integer_token(next, &token);
      }
      else { /* if not digits it is part of a symbol */
        next = read_symbol_token(next, &token);
      }
      break;

      /* read keyword */
    case ':':
      next = read_keyword_token(next, &token);
      break;

      /* read anything else as a symbol */
    default:
      next = read_symbol_token(next, &token);
      break;
    }

    if (!token) {
      /* if no token was read (whitespace or comments)
         continue the loop */
      continue;
    }
    else {

      if (token->error) {
        /* report any errors with an early return */
        reader = reader_append(reader, token);
        reader->error = token->error;
        return reader;
      }
      /* otherwise append the token and continue */
      reader = reader_append(reader, token);
    }
  }
  return reader;
}

char* read_fixed_length_token(char* current, Token** ptoken, int n) {

  *ptoken = token_allocate(current, n, TOKEN_SPECIAL_CHARACTER, NULL);
  return (current + n);
}

char* read_terminated_token (char* current, Token** ptoken, int token_type) {

  static char* const terminating_characters = " ,[](){};\n";

  /* search for first terminating character */
  char* end = strpbrk(current, terminating_characters);

  /* if terminating character is not found it implies the end of the string */
  long token_length = !end ? strlen(current) : (end - current);

  /* next token starts with the terminating character */
  *ptoken = token_allocate(current, token_length, token_type, NULL);
  return (current + token_length);
}

char* read_symbol_token (char* current, Token** ptoken) {

  char* next = read_terminated_token(current, ptoken, TOKEN_SYMBOL);

  /* check for reserved symbols */
  if (strcmp((*ptoken)->data, SYMBOL_NIL) == 0) {
    (*ptoken)->type = TOKEN_NIL;
  }
  else if (strcmp((*ptoken)->data, SYMBOL_TRUE) == 0) {
    (*ptoken)->type = TOKEN_TRUE;
  }
  else if (strcmp((*ptoken)->data, SYMBOL_FALSE) == 0) {
    (*ptoken)->type = TOKEN_FALSE;
  }

  /* TODO: check for invalid characters */
  return next;
}


char* read_keyword_token (char* current, Token** ptoken) {

  /* TODO: check for invalid characters */
  return read_terminated_token(current + 1, ptoken, TOKEN_KEYWORD);
}

char* read_number_token(char* current, Token** ptoken) {

  int has_decimal_point = 0;

  char* next = read_terminated_token(current, ptoken, TOKEN_INTEGER);
  long token_length = next - current;

  /* first char is either digit or '+' or '-'
     check the rest consists of valid characters */
  for (long i = 1; i < token_length; i++) {

    if ((*ptoken)->data[i] == '.' && has_decimal_point) {
      (*ptoken)->error = "Invalid character reading number";
      break;
    }
    else if ((*ptoken)->data[i] == '.' && !has_decimal_point) {
      has_decimal_point = 1;
      (*ptoken)->type = TOKEN_FLOAT;
      break;
    }
    else if (!(isdigit((*ptoken)->data[i]))) {
      (*ptoken)->error = "Invalid character reading number";
      break;
    }
  }
  return next;
}

char* read_string_token(char* current, Token** ptoken) {

  char *start, *end, *error = NULL;
  long token_length = 0;

  start = current + 1;

  while(1) {
    end = strchr(start, '"'); /* find the next " character */

    /* handle failure to find closing quotes - implies end of input has been reached */
    if (!end) {
      end = current + strlen(current);
      token_length =  strlen(current);

      error = "EOF reached with unterminated string";
      break;
    }
    /* if the character preceding the " is a '\' character (escape), need to check if it is escaping the " and if it
       is then keep searching from the next character */
    else if ( *(end - 1) == '\\') {

      char* back_ptr = end - 1;
      while (*back_ptr == '\\') {
        back_ptr--; /* back up to count the escape characters '\' */
      }

      long escape_chars = (end - 1) - back_ptr;

      if (escape_chars % 2 == 1) { /* odd number of '\' chars means " is not quoted */
        start = end + 1; /* so keep searching */
      } else {
        /* even number of '\' characters means we found the terminating quote mark */
        token_length =  (end - current - 1); /* quotes are excluded from string token */
        break;
      }
    }
    else {
      token_length =  (end - current - 1); /* quotes are excluded from string token */
      break;
    }
  }

  char* unescaped_string = unescape_string(current + 1, token_length);
  *ptoken = token_allocate(unescaped_string, strlen(unescaped_string), TOKEN_STRING, error);

  return (end + 1);
}

char* read_comment_token(char* current, Token** ptoken) {
  /* comment includes all remaining characters to the next newline */

  /* search for newline character */
  char* end = strchr(current, 0x0A);

  /* if newline is not found it implies the end of string is reached */
  long token_chars = !end ? strlen(current) : (end - current);

  *ptoken = token_allocate(current, token_chars, TOKEN_COMMENT, NULL);

  return (current + token_chars + 1); /* next token starts with the char after the newline */
}

MalType* read_form(Reader* reader) {

  if (reader->token_count > 0) {

    Token* tok = reader_peek(reader);
    if (tok->type == TOKEN_SPECIAL_CHARACTER) {

      switch(tok->data[0]) {

      case '(':
        return read_list(reader);
        break;

      case '[':
        return read_vector(reader);
        break;

      case '{':
        return read_hashmap(reader);
        break;

      case '\'':
        /* create and return a MalType list (quote read_form) */
        return make_symbol_list(reader, SYMBOL_QUOTE);
        break;

      case '`':
        /* create and return a MalType list (quasiquote read_form) */
        return make_symbol_list(reader, SYMBOL_QUASIQUOTE);
        break;

      case '~':
        if (tok->data[1] == '@') {
          /* create and return a MalType list (splice-unquote read_form) */
          return make_symbol_list(reader, SYMBOL_SPLICE_UNQUOTE);
        }
        else {
          /* create and return a MalType list (unquote read_form) */
          return make_symbol_list(reader, SYMBOL_UNQUOTE);
        }
      case '@':
        /* create and return a MalType list (deref read_form) */
        return make_symbol_list(reader, SYMBOL_DEREF);

      case '^':
        /* create and return a MalType list (with-meta <second-form> <first-form>
           where first form should ne a metadata map and second form is somethingh
           that can have metadata attached */
          reader_next(reader);

          /* grab the components of the list */
          MalType* symbol = make_symbol(SYMBOL_WITH_META);
          MalType* first_form = read_form(reader);
          MalType* second_form = read_form(reader);

          /* push the symbol and the following forms onto a list */
          list lst = NULL;
          lst = list_push(lst, symbol);
          lst = list_push(lst, second_form);
          lst = list_push(lst, first_form);
          lst = list_reverse(lst);

          return make_list(lst);

      default:
        /* shouldn't happen */
        return make_error_fmt("Reader error: Unknown special character '%c'", tok->data[0]);
      }

    } else { /* Not a special character */
      return read_atom(reader);
    }
  }
  else { /* no tokens */
    return NULL;
  }
}

MalType* read_list(Reader* reader) {

  MalType* retval = read_matched_delimiters(reader, '(', ')' );

  if (is_error(retval)) {
    retval = make_error("Reader error: unbalanced parenthesis '()'");
  }
  else {
    retval->type = MALTYPE_LIST;
  }
  return retval;
}

MalType* read_vector(Reader* reader) {

  MalType* retval = read_matched_delimiters(reader, '[', ']' );

  if (is_error(retval)) {
    retval = make_error("Reader error: unbalanced brackets '[]'");
  }
  else {
    retval->type = MALTYPE_VECTOR;
  }
  return retval;
}

MalType* read_hashmap(Reader* reader) {

  MalType* retval = read_matched_delimiters(reader, '{', '}' );

  if (is_error(retval)) {
    retval = make_error("Reader error: unbalanced braces '{}'");
  }
  else if (list_count(retval->value.mal_list)%2 != 0) {
    retval = make_error("Reader error: missing value in map literal");
  }
  else {
    retval->type = MALTYPE_HASHMAP;
  }
  return retval;
}

MalType* read_matched_delimiters(Reader* reader, char start_delimiter, char end_delimiter) {
/* TODO: separate implementation of hashmap and vector */

  Token* tok = reader_next(reader);
  list lst = NULL;

  if (reader_peek(reader)->data[0] == end_delimiter) {
    reader_next(reader);
    return make_list(NULL);
  }
  else {
    while (tok->data[0] != end_delimiter) {

        MalType* val = read_form(reader);
        lst = list_push(lst, (gptr)val);

        tok = reader_peek(reader);

        if (!tok) {
          /* unbalanced parentheses */
          return make_error("");
        }
      }
    reader_next(reader);

    return  make_list(list_reverse(lst));
  }
}

MalType* read_atom(Reader* reader) {

  Token* tok = reader_next(reader);

  switch (tok->type) {

  case TOKEN_SPECIAL_CHARACTER:
    return make_symbol(tok->data);
    break;

  case TOKEN_COMMENT:
    return make_error("Error: comment found in token strea");
    break;

  case TOKEN_STRING:
    return make_string(tok->data);
    break;

  case TOKEN_INTEGER:
    return make_integer(strtol(tok->data, NULL, 10));
    break;

  case TOKEN_FLOAT:
    return make_float(atof(tok->data));
    break;

  case TOKEN_SYMBOL:
    return make_symbol(tok->data);
    break;

  case TOKEN_KEYWORD:
    return make_keyword(tok->data);
    break;

  case TOKEN_TRUE:
    return make_true();
    break;

  case TOKEN_FALSE:
    return make_false();
    break;

  case TOKEN_NIL:
    return make_nil();
    break;
  }
  return make_error("Reader error: Unknown atom type");
}

MalType* make_symbol_list(Reader* reader, char* symbol_name) {

  reader_next(reader);
  list lst = NULL;

  /* push the symbol and the following form onto the list */
  lst = list_push(lst, make_symbol(symbol_name));
  lst = list_push(lst, read_form(reader));

  return make_list(list_reverse(lst));
}

Token* token_allocate(char* str, long num_chars, int type, char* error) {

  /* allocate space for the string */
  char* data = GC_MALLOC(sizeof(*data) * num_chars + 1); /* include space for null byte */
  strncpy (data, str, num_chars);                        /* copy num_chars characters into data */
  data[num_chars] = '\0';                                /* manually add the null byte */

  /* allocate space for the token struct */
  Token* token = GC_MALLOC(sizeof(*token));
  token->data = data;
  token->type = type;
  token->error = error;

  return token;
}

char* unescape_string(char* str, long length) {

  char* dest = GC_MALLOC(sizeof(*dest)*length + 1);

  long j = 0;
  for (long i = 0; i < length; i++) {

    /* look for the quoting character */
    if (str[i] == '\\') {

      switch (str[i+1]) {

        /* replace '\"' with normal '"' */
      case '"':
        dest[j++]='"';
        i++; /* skip extra char */
        break;

        /* replace '\n' with newline 0x0A */
      case 'n':
        dest[j++]= 0x0A;
        i++; /* skip extra char */
        break;

        /* replace '\\' with '\' */
      case '\\':
        dest[j++]= '\\';
        i++; /* skip extra char */
        break;

      default:
        /* just a '\' symbol so copy it */
              dest[j++]='\\';
      }
    }
    /* not a quote so copy it */
    else {
        dest[j++] = str[i];
    }
  }
  dest[j] = '\0';

  return dest;
}
