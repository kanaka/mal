#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <gc.h>

#include "libs/hashmap/hashmap.h"
#include "printer.h"
#include "reader.h"

#define SYMBOL_NIL "nil"
#define SYMBOL_TRUE "true"
#define SYMBOL_FALSE "false"
#define SYMBOL_QUOTE "quote"
#define SYMBOL_QUASIQUOTE "quasiquote"
#define SYMBOL_UNQUOTE  "unquote"
#define SYMBOL_SPLICE_UNQUOTE "splice-unquote"
#define SYMBOL_DEREF "deref"
#define SYMBOL_WITH_META "with-meta"

#define DEBUG(...)
// #define DEBUG(fmt, ...) printf("READER: %s \"%s\": " fmt "\n", __func__, *reader, ## __VA_ARGS__)

typedef const char** Reader;

MalType read_form(Reader reader);
MalType read_with_meta(Reader reader);
MalType read_string(Reader reader);
MalType read_number(Reader reader);
const char* read_symbol (Reader reader);
MalType read_matched_delimiters(Reader reader, char start_delimiter,
                                char end_delimiter, function_t constructor);
void skip_spaces(Reader reader);
MalType make_symbol_list(Reader reader, const char* symbol_name);

void skip_spaces(Reader reader) {

  while(true) {
    if(**reader == ';') {
      do {
        (*reader)++;
        if(**reader == 0x00) return;
      } while(**reader != 0x0A);
    }
    else if((**reader != ',') && !isspace(**reader)) {
      return;
    }
    (*reader)++;
  }
}

MalType read_str(const char* source) {

  MalType result = read_form(&source);
  if(is_error(result)) return result;
  skip_spaces(&source);
  if(*source)
    return make_error_fmt("trailing characters (after %M): %s",
                          result, source);
  return result;
}

const char* read_symbol (Reader reader) {

  DEBUG();

  const char* start = *reader;
  while(true) {
    switch(**reader) {
    case 0:
      return start;
    case '[':
    case '{':
    case '(':
    case ']':
    case '}':
    case ')':
    case '\'':
    case '@':
    case '`':
    case '^':
    case '~':
    case '"':
    case ',':
    case ';':
      goto finished;
    default:
      if(isspace(**reader))
        goto finished;
      (*reader)++;
    }
  }
 finished:
  size_t len = *reader - start;
  char* result = GC_MALLOC(len + 1);
  strncpy(result, start, len);
  return result;
}

MalType read_form(Reader reader) {

  DEBUG();

  skip_spaces(reader);
  switch (**reader) {
  case 0:
    return make_error_fmt("input string is empty");
  case '[':
    return read_matched_delimiters(reader, '[', ']', make_vector);
  case '{':
    return read_matched_delimiters(reader, '{', '}', mal_hash_map);
  case '(':
    return read_matched_delimiters(reader, '(', ')', make_list);
  case ']':
  case '}':
  case ')':
    return make_error_fmt("unmatched '%c'", **reader);
  case '\'':
    return make_symbol_list(reader, SYMBOL_QUOTE);
  case '@':
    return make_symbol_list(reader, SYMBOL_DEREF);
  case '`':
    return make_symbol_list(reader, SYMBOL_QUASIQUOTE);
  case '^':
    return read_with_meta(reader);
  case '~':
    if(*(*reader + 1) == '@') {
      (*reader)++;
      return make_symbol_list(reader, SYMBOL_SPLICE_UNQUOTE);
    }
    return make_symbol_list(reader, SYMBOL_UNQUOTE);
  case '"':
    return read_string(reader);
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
    return read_number(reader);
  case '+':
  case '-':
    if(isdigit(*(*reader + 1)))
      return read_number(reader);
    else
      return make_symbol(read_symbol(reader));
  case ':':
    (*reader)++;
    return make_keyword(read_symbol(reader));
  default:
    {
      const char* sym = read_symbol(reader);
      if(!strcmp(sym, SYMBOL_NIL))   return make_nil();
      if(!strcmp(sym, SYMBOL_FALSE)) return make_false();
      if(!strcmp(sym, SYMBOL_TRUE))  return make_true();
      return make_symbol(sym);
    }
  }
}

MalType read_number(Reader reader) {

  DEBUG();

  const char* start = *reader;
  // Skip the initial character, which is a digit or a +- sign
  // (followed by a digit).
  (*reader)++;

  int has_decimal_point = 0;

  while(true) {
    if(**reader == '.') {
      if(has_decimal_point) break;
      has_decimal_point = true;
      (*reader)++;
    }
    else if(isdigit(**reader))
      (*reader)++;
    else
      break;
  }

  size_t len = *reader - start;
  char buffer[len + 1];
  strncpy(buffer, start, len);
  buffer[len] = 0;

  if(has_decimal_point)
    return make_float(atof(buffer));
  else
    return make_integer(atol(buffer));
}

MalType read_with_meta(Reader reader) {

          DEBUG();

        /* create and return a MalType list (with-meta <second-form> <first-form>
           where first form should ne a metadata map and second form is somethingh
           that can have metadata attached */
          (*reader)++;

          /* grab the components of the list */
          MalType symbol = make_symbol(SYMBOL_WITH_META);
          MalType first_form = read_form(reader);
          if(is_error(first_form)) return first_form;
          MalType second_form = read_form(reader);
          if(is_error(second_form)) return second_form;

          /* push the symbol and the following forms onto a list */
          list lst = NULL;
          lst = list_push(lst, first_form);
          lst = list_push(lst, second_form);
          lst = list_push(lst, symbol);

          return make_list(lst);
 }

MalType read_matched_delimiters(Reader reader, char start_delimiter,
                                char end_delimiter, function_t constructor) {
/* TODO: separate implementation of hashmap and vector */

  (*reader)++;
  list lst = NULL;
  list* lst_last = &lst;

    while(true) {
      DEBUG("searching '%c', already read: % N", end_delimiter, lst);
      skip_spaces(reader);

      if(!**reader) {
        /* unbalanced parentheses */
        return make_error_fmt("unbalanced '%c'", start_delimiter);
      }

      if(**reader == end_delimiter)
        break;
      MalType val = read_form(reader);
      if(is_error(val)) return val;
      *lst_last = list_push(NULL, val);
      lst_last = &(*lst_last)->next;
    }
  (*reader)++;
  return constructor(lst);
}

MalType make_symbol_list(Reader reader, const char* symbol_name) {

  DEBUG();

  (*reader)++;
  list lst = NULL;

  /* push the symbol and the following form onto the list */
  MalType form = read_form(reader);
  if(is_error(form)) return form;
  lst = list_push(lst, form);
  lst = list_push(lst, make_symbol(symbol_name));

  return make_list(lst);
}

MalType read_string(Reader reader) {

  DEBUG();

  (*reader)++; // initial '"'
  size_t count = 0;

  //  Compute the length.
  for(const char* p=*reader; *p!='"'; p++) {
    if(!*p)
      return make_error_fmt("unbalanced '\"'");
    if(*p == '\\') {
      p++;
      switch(*p) {
      case 0:
        return make_error_fmt("incomplete \\ escape sequence");
      case '\\':
      case 'n':
      case '"':
        break;

      default:
        return make_error_fmt("invalid escape sequence '\\%c'", *p);
      }
    }
    count++;
  }

  //  Copy/unescape the characters, add final 0.
  char* result = GC_MALLOC(count + 1);
  const char* src;
  char* dst = result;
  for(src=*reader; *src!='"'; src++) {
    if(*src == '\\') {
      src++;
      if(*src == 'n') {
        *dst++ = 0x0A;
        continue;
      }
    }
    *dst++ = *src;
  }
  *dst = 0;

  *reader = src + 1;
  return make_string(result);
}
