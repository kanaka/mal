#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include <gc.h>

#include "hashmap.h"
#include "printer.h"
#include "reader.h"
#include "linked_list.h"
#include "vector.h"
#include "error.h"

#define SYMBOL_NIL "nil"
#define SYMBOL_TRUE "true"
#define SYMBOL_FALSE "false"

#ifdef DEBUG_READER
#  define DEBUG(fmt, ...) printf("READER: %s \"%s\": " fmt "\n", __func__, *reader, ## __VA_ARGS__)
#else
#  define DEBUG(...)
#endif

typedef const char** Reader;

MalType read_form(Reader reader);
MalType read_with_meta(Reader reader);
MalType read_string(Reader reader);
MalType read_number(Reader reader);
const char* read_symbol (Reader reader);
MalType read_list(Reader reader);
MalType read_vector(Reader reader);
MalType read_map(Reader reader);
void skip_spaces(Reader reader);
MalType make_symbol_list(Reader reader, MalType symbol_name);

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
  if(mal_error) return NULL;
  skip_spaces(&source);
  if(*source)
    make_error("reader: trailing characters (after %M): %s",
                          result, source);
  return result;
}

const char* read_symbol (Reader reader) {

  DEBUG();

  const char* start = *reader;
  while(!isspace(**reader)) {
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
      (*reader)++;
    }
  }
 finished:
  size_t len = *reader - start;
  char* result = GC_MALLOC(len + 1);
  strncpy(result, start, len);
  assert(!result[len]);
  return result;
}

MalType read_form(Reader reader) {

  DEBUG();

  skip_spaces(reader);
  switch (**reader) {
  case 0:
    make_error("reader: input string is empty");
  case '[':
    return read_vector(reader);
    // Implicit error propagation
  case '{':
    return read_map(reader);
    // Implicit error propagation
  case '(':
    return read_list(reader);
    // Implicit error propagation
  case ']':
  case '}':
  case ')':
    make_error("reader: unmatched '%c'", **reader);
  case '\'':
    return make_symbol_list(reader, SYMBOL_QUOTE);
  case '@':
    return make_symbol_list(reader, SYMBOL_DEREF);
  case '`':
    return make_symbol_list(reader, SYMBOL_QUASIQUOTE);
  case '^':
    return read_with_meta(reader);
    // Implicit error propagation
  case '~':
    if(*(*reader + 1) == '@') {
      (*reader)++;
      return make_symbol_list(reader, SYMBOL_SPLICE_UNQUOTE);
    }
    return make_symbol_list(reader, SYMBOL_UNQUOTE);
  case '"':
    return read_string(reader);
    // Implicit error propagation
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

  bool has_decimal_point = false;

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
          MalType symbol = SYMBOL_WITH_META;
          MalType first_form = read_form(reader);
          if(mal_error) return NULL;
          MalType second_form = read_form(reader);
          if(mal_error) return NULL;

          /* push the symbol and the following forms onto a list */
          list lst = NULL;
          lst = list_push(lst, first_form);
          lst = list_push(lst, second_form);
          lst = list_push(lst, symbol);

          return make_list(lst);
 }

MalType read_list(Reader reader) {

  (*reader)++;
  list lst = NULL;
  list* lst_last = &lst;

    while(true) {
      DEBUG("searching ')', already read: %N", lst);
      skip_spaces(reader);

      if(!**reader) {
        /* unbalanced parentheses */
        make_error("reader: unbalanced '('");
      }

      if(**reader == ')')
        break;
      MalType val = read_form(reader);
      if(mal_error) return NULL;
      *lst_last = list_push(NULL, val);
      lst_last = &(*lst_last)->next;
    }
  (*reader)++;
  return make_list(lst);
}

MalType read_vector(Reader reader) {
  (*reader)++;
  size_t capacity = 10;
  struct vector* v = vector_new(capacity);
  while(true) {
    DEBUG("searching ']'");
    skip_spaces(reader);
    if (!**reader) {
      make_error("reader: unbalanced '['");
    }
    if (**reader == ']')
      break;
    MalType val = read_form(reader);
    if (mal_error) return NULL;
    vector_append(&capacity, &v, val);
  }
  (*reader)++;
  return make_vector(v);
}

MalType read_map(Reader reader) {
  (*reader)++;
  struct map* map = map_empty();
  while(true) {
    DEBUG("searching '}' or key");
    skip_spaces(reader);
    if (!**reader) {
      make_error("reader: unbalanced '{'");
    }
    if (**reader == '}')
      break;
    MalType key = read_form(reader);
    if (mal_error) return NULL;
    check_type("reading map literal", MALTYPE_KEYWORD | MALTYPE_STRING, key);
    DEBUG("searching map value for %M", key);
    skip_spaces(reader);
    if (!**reader) {
      make_error("reader: unbalanced '{'");
    }
    if (**reader == '}') {
      make_error("reader: odd count of bindings in map litteral");
    }
    MalType value = read_form(reader);
    if (mal_error) return NULL;
    map = hashmap_put(map, key, value);
  }
  (*reader)++;
  return make_hashmap(map);
}

MalType make_symbol_list(Reader reader, MalType symbol) {

  DEBUG();

  (*reader)++;
  list lst = NULL;

  /* push the symbol and the following form onto the list */
  MalType form = read_form(reader);
  if(mal_error) return NULL;
  lst = list_push(lst, form);
  lst = list_push(lst, symbol);

  return make_list(lst);
}

MalType read_string(Reader reader) {

  DEBUG();

  (*reader)++; // initial '"'
  size_t count = 0;

  //  Compute the length.
  for(const char* p=*reader; *p!='"'; p++) {
    if(!*p)
      make_error("reader: unbalanced '\"'");
    if(*p == '\\') {
      p++;
      switch(*p) {
      case 0:
        make_error("reader: incomplete \\ escape sequence");
      case '\\':
      case 'n':
      case '"':
        break;

      default:
        make_error("reader: incomplete escape sequence '\\%c'", *p);
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
