#include <stdarg.h>
#include <stdio.h>
#include <gc.h>
#include "types.h"

#define ERROR_BUFFER_SIZE 128

MalType THE_TRUE = {MALTYPE_TRUE, 0, 0, {0}};
MalType THE_FALSE = {MALTYPE_FALSE, 0, 0, {0}};
MalType THE_NIL = {MALTYPE_NIL, 0, 0, {0}};


inline int is_sequential(MalType* val) {
  return (val->type == MALTYPE_LIST || val->type == MALTYPE_VECTOR);
}

inline int is_self_evaluating(MalType* val) {
  return (val->type == MALTYPE_KEYWORD || val->type == MALTYPE_INTEGER ||
	  val->type == MALTYPE_FLOAT || val->type == MALTYPE_STRING ||
	  val->type == MALTYPE_TRUE || val->type == MALTYPE_FALSE ||
	  val->type == MALTYPE_NIL);
}

inline int is_list(MalType* val) {
  return (val->type == MALTYPE_LIST);
}

inline int is_vector(MalType* val) {
  return (val->type == MALTYPE_VECTOR);
}

inline int is_hashmap(MalType* val) {
  return (val->type == MALTYPE_HASHMAP);
}

inline int is_nil(MalType* val) {
  return (val->type == MALTYPE_NIL);
}

inline int is_string(MalType* val) {
  return (val->type == MALTYPE_STRING);
}

inline int is_integer(MalType* val) {
  return (val->type == MALTYPE_INTEGER);
}

inline int is_float(MalType* val) {
  return (val->type == MALTYPE_FLOAT);
}

inline int is_number(MalType* val) {
  return (val->type == MALTYPE_INTEGER || val->type == MALTYPE_FLOAT);
}

inline int is_true(MalType* val) {
  return (val->type == MALTYPE_TRUE);
}

inline int is_false(MalType* val) {
  return (val->type == MALTYPE_FALSE);
}

inline int is_symbol(MalType* val) {
  return (val->type == MALTYPE_SYMBOL);
}

inline int is_keyword(MalType* val) {
  return (val->type == MALTYPE_KEYWORD);
}

inline int is_atom(MalType* val) {
  return (val->type == MALTYPE_ATOM);
}

inline int is_error(MalType* val) {
  return (val->type == MALTYPE_ERROR);
}

inline int is_callable(MalType* val) {
  return (val->type == MALTYPE_FUNCTION || val->type == MALTYPE_CLOSURE);
}

inline int is_function(MalType* val) {
  return (val->type == MALTYPE_FUNCTION);
}

inline int is_closure(MalType* val) {
  return (val->type == MALTYPE_CLOSURE);
}

inline int is_macro(MalType* val) {
  return (val->is_macro);
}


MalType* make_symbol(char* value) {

  MalType* mal_val = GC_MALLOC(sizeof(*mal_val));
  mal_val->type = MALTYPE_SYMBOL;
  mal_val->value.mal_symbol = value;
  mal_val->metadata = NULL;

  return mal_val;
}

MalType* make_integer(long value) {

  MalType* mal_val = GC_MALLOC(sizeof(*mal_val));
  mal_val->type = MALTYPE_INTEGER;
  mal_val->value.mal_integer = value;
  mal_val->metadata = NULL;

  return mal_val;
}

MalType* make_float(double value) {

  MalType* mal_val = GC_MALLOC(sizeof(*mal_val));
  mal_val->type = MALTYPE_FLOAT;
  mal_val->value.mal_float = value;
  mal_val->metadata = NULL;

  return mal_val;
}

MalType* make_keyword(char* value) {

  MalType* mal_val = GC_MALLOC(sizeof(*mal_val));
  mal_val->type = MALTYPE_KEYWORD;
  mal_val->value.mal_keyword = value;
  mal_val->metadata = NULL;

  return mal_val;
}

MalType* make_string(char* value) {

  MalType* mal_val = GC_MALLOC(sizeof(*mal_val));
  mal_val->type = MALTYPE_STRING;
  mal_val->value.mal_string = value;
  mal_val->metadata = NULL;

  return mal_val;
}

MalType* make_list(list value) {

  MalType* mal_val = GC_MALLOC(sizeof(*mal_val));
  mal_val->type = MALTYPE_LIST;
  mal_val->value.mal_list = value;
  mal_val->metadata = NULL;

  return mal_val;
}

MalType* make_vector(list value) {

  MalType* mal_val = GC_MALLOC(sizeof(*mal_val));
  mal_val->type = MALTYPE_VECTOR;
  mal_val->value.mal_list = value;
  mal_val->metadata = NULL;

  return mal_val;
}

MalType* make_hashmap(list value) {

  MalType* mal_val = GC_MALLOC(sizeof(*mal_val));
  mal_val->type = MALTYPE_HASHMAP;
  mal_val->value.mal_list = value;
  mal_val->metadata = NULL;

  return mal_val;
}

MalType* make_atom(MalType* value) {

  MalType* mal_val = GC_MALLOC(sizeof(*mal_val));
  mal_val->type = MALTYPE_ATOM;
  mal_val->value.mal_atom = value;
  mal_val->metadata = NULL;

  return mal_val;
}

MalType* make_function(MalType*(*fn)(list args)) {

  MalType* mal_val = GC_MALLOC(sizeof(*mal_val));
  mal_val->type = MALTYPE_FUNCTION;
  mal_val->value.mal_function = fn;
  mal_val->is_macro = 0;
  mal_val->metadata = NULL;

  return mal_val;
}

MalType* make_closure(Env* env, MalType* parameters, MalType* definition, MalType* more_symbol) {

  MalType* mal_val = GC_MALLOC(sizeof(*mal_val));
  mal_val->type = MALTYPE_CLOSURE;
  mal_val->metadata = NULL;

  /* Allocate memory for embedded struct */
  MalClosure* mc = GC_MALLOC(sizeof(*mc));
  mc->env = env;
  mc->parameters = parameters;
  mc->definition = definition;
  mc->more_symbol = more_symbol;

  mal_val->is_macro = 0;
  mal_val->value.mal_closure = mc;
  return mal_val;
}

inline MalType* make_true() {
  return &THE_TRUE;
}

inline MalType* make_false() {
  return &THE_FALSE;
}

inline MalType* make_nil() {
  return &THE_NIL;
}

MalType* make_error(char* msg) {

  MalType* mal_string = GC_MALLOC(sizeof(*mal_string));
  mal_string->type = MALTYPE_STRING;
  mal_string->value.mal_string = msg;

  MalType* mal_val = GC_MALLOC(sizeof(*mal_val));
  mal_val->type = MALTYPE_ERROR;
  mal_val->value.mal_error = mal_string;
  mal_val->metadata = NULL;

  return mal_val;
}

MalType* make_error_fmt(char* fmt, ...) {

  va_list argptr;
  va_start(argptr, fmt);

  char* buffer = GC_MALLOC(sizeof(*buffer) * ERROR_BUFFER_SIZE);

  long n = vsnprintf(buffer, ERROR_BUFFER_SIZE, fmt, argptr);
  va_end(argptr);

  if (n > ERROR_BUFFER_SIZE) {
    va_start(argptr, fmt);

    buffer = GC_REALLOC(buffer, sizeof(*buffer) * n);
    vsnprintf(buffer, n, fmt, argptr);

    va_end(argptr);
  }
  return make_error(buffer);
}

MalType* wrap_error(MalType* value) {

  MalType* mal_error = GC_MALLOC(sizeof(*mal_error));
  mal_error->type = MALTYPE_ERROR;
  mal_error->metadata = NULL;
  mal_error->value.mal_error = value;

  return mal_error;
}

MalType* copy_type(MalType* value) {

  MalType* new_val = GC_MALLOC(sizeof(*new_val));

  new_val->type = value->type;
  new_val->is_macro = value->is_macro;
  new_val->value = value->value;
  new_val->metadata = value->metadata;

  return new_val;
}
