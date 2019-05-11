#include <ctype.h>
#include <gc.h>
#include <pcre.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include "printer.h"
#include "reader.h"
#include "types.h"
#include "util.h"

char* reader_next(Reader* reader) {
  if (reader->token) {
    char* str = reader->token->str;
    reader->token = reader->token->next;
    if (reader->token && strlen(reader->token->str) == 0) {
      reader->token = NULL;
    }
    return str;
  } else {
    return NULL;
  }
}

char* reader_peek(Reader* reader) {
  if (reader->token) {
    return reader->token->str;
  } else {
    return NULL;
  }
}

Token* tokenize(char* code) {
  pcre *reCompiled;
  pcre_extra *pcreExtra;
  int pcreExecRet;
  int subStrVec[30];
  const char *pcreErrorStr;
  int pcreErrorOffset;
  const char *psubStrMatchStr;
  size_t offset = 0;
  unsigned int len = strlen(code), l;
  Token *firstToken = NULL, *lastToken = NULL, *token;
  char* str;

  reCompiled = pcre_compile(PATTERN, 0, &pcreErrorStr, &pcreErrorOffset, NULL);

  if(reCompiled == NULL) {
    printf("ERROR: Could not compile regex: %s\n", pcreErrorStr);
    exit(1);
  }

  pcreExtra = pcre_study(reCompiled, PCRE_EXTENDED, &pcreErrorStr);

  if(pcreErrorStr != NULL) {
    printf("ERROR: Could not study regex: %s\n", pcreErrorStr);
    exit(1);
  }

  while (offset < len && (pcreExecRet = pcre_exec(reCompiled, pcreExtra, code, len, offset, 0, subStrVec, 30))) {
    if(pcreExecRet < 0) {
      fprintf(stderr, "There was an error parsing the code\n");
      exit(1);
    }

    if(pcreExecRet == 0) {
      printf("Too many substrings were found to fit in subStrVec!\n");
      exit(1);
    }

    token = GC_MALLOC(sizeof(Token));
    pcre_get_substring(code, subStrVec, pcreExecRet, 1, &(psubStrMatchStr));
    l = strlen(psubStrMatchStr);
    if (l == 0) {
      break;
    }
    str = string((char*)psubStrMatchStr);
    token->str = str;
    token->next = NULL;
    if (lastToken) {
      lastToken->next = token;
    }
    lastToken = token;
    if (!firstToken) {
      firstToken = token;
    }

    offset = subStrVec[1];
  }

  pcre_free(reCompiled);
      
  if(pcreExtra != NULL) {
#ifdef PCRE_CONFIG_JIT
    pcre_free_study(pcreExtra);
#else
    pcre_free(pcreExtra);
#endif
  }

  return firstToken;
}

MalType* read_str(char* code) {
  Reader* reader = GC_MALLOC(sizeof(Reader));
  reader->token = tokenize(code);
  MalType *atom = read_form(reader);
  if (is_blank_line(atom)) {
    return mal_nil();
  } else {
    return atom;
  }
}

MalType* read_form(Reader* reader) {
  char *token = reader_peek(reader);
  if (!token || strlen(token) == 0) {
    return mal_nil();
  } else {
    switch (*token) {
      case ';':
        reader_next(reader);
        return mal_blank_line();
      case '(':
        return read_list(reader);
      case '[':
        return read_vector(reader);
      case '{':
        return read_hashmap(reader);
      case '"':
        return read_string(reader);
      case ':':
        return read_keyword(reader);
      case '\'':
        return read_quote(reader, "quote");
      case '~':
        if (strlen(token) > 1 && *(token + 1) == '@') {
          return read_quote(reader, "splice-unquote");
        } else {
          return read_quote(reader, "unquote");
        }
      case '`':
        return read_quote(reader, "quasiquote");
      case '@':
        return read_quote(reader, "deref");
      case '^':
        return read_with_meta(reader);
      default:
        return read_atom(reader);
    }
  }
}

MalType* read_keyword(Reader *reader) {
  char *token = reader_next(reader);
  return mal_keyword(token + 1);
}

MalType* read_quote(Reader *reader, const char *expanded) {
  reader_next(reader); // consume quote
  MalType *atom = read_form(reader);
  MalType *c2 = mal_cons(atom, mal_empty());
  MalType *c1 = mal_cons(mal_symbol((char*)expanded), c2);
  return c1;
}

MalType* read_with_meta(Reader *reader) {
  reader_next(reader); // consume '^'
  MalType *metadata = read_form(reader);
  MalType *value = read_form(reader);
  MalType *c3 = mal_cons(metadata, mal_empty());
  MalType *c2 = mal_cons(value, c3);
  MalType *c1 = mal_cons(mal_symbol("with-meta"), c2);
  return c1;
}

MalType* read_vector(Reader *reader) {
  MalType *atom;
  MalType *vec = mal_vector(-1);
  reader_next(reader); // consume '['
  char *token = reader_peek(reader);
  while (token && *token != ']') {
    atom = read_form(reader);
    token = reader_peek(reader);
    if (is_blank_line(atom)) continue;
    mal_vector_push(vec, atom);
  }
  if (token) {
    reader_next(reader); // consume ']'
  } else {
    printf("EOF\n");
  }
  return vec;
}


MalType* read_hashmap(Reader *reader) {
  reader_next(reader); // consume '{'
  MalType *key, *val;
  MalType *map = mal_hashmap();
  char *token = reader_peek(reader);
  while (token && *token != '}') {
    key = read_form(reader);
    token = reader_peek(reader);
    if (token && *token != '}') {
      val = read_form(reader);
      mal_hashmap_put(map, key, val);
    } else {
      printf("Odd number of hashmap items!\n");
      break;
    }
    token = reader_peek(reader);
  }
  if (token) {
    reader_next(reader); // consume '}'
  } else {
    printf("EOF\n");
  }
  return map;
}

MalType* read_string(Reader *reader) {
  char *token = reader_next(reader);
  size_t len = strlen(token);
  char *str = GC_MALLOC(len + 1);
  size_t index = 0;
  int saw_quotes = 0;
  char unescaped;
  for (size_t i=0; i<len; i++) {
    switch (token[i]) {
      case '"':
        saw_quotes++;
        break;
      case '\\':
        i++;
        unescaped = unescape_char(token, &i, len - 1); // note: len-1 because of closing quote
        if (unescaped) {
          str[index++] = unescaped;
          break;
        } else {
          return mal_error(mal_string("Invalid escape sequence in string"));
        }
      default:
        str[index++] = token[i];
    }
  }
  str[index] = 0;
  if (saw_quotes != 2) {
    printf("EOF\n");
  }
  return mal_string(str);
}

char unescape_char(char *token, size_t *i, size_t len) {
  char c = token[*i];
  if (c == 'n') {
    return 10; // newline
  } else if (c == 'x') {
    char seq[3];
    if ((*i)+2 < len) {
      seq[0] = token[++*i];
      seq[1] = token[++*i];
      seq[2] = 0;
      int val;
      sscanf(seq, "%x", &val);
      return (char)val;
    } else {
      return 0;
    }
  } else {
    return c;
  }
}

MalType* read_list(Reader *reader) {
  MalType *atom, *last_cons = NULL, *first_cons = NULL, *cons;
  reader_next(reader); // consume '('
  char *token = reader_peek(reader);
  while (token && *token != ')') {
    atom = read_form(reader);
    token = reader_peek(reader);
    if (is_blank_line(atom)) continue;
    cons = mal_cons(atom, NULL);
    if (!first_cons) first_cons = cons;
    if (last_cons) last_cons->cdr = cons;
    last_cons = cons;
  }
  if (token) {
    reader_next(reader); // consume ')'
  } else {
    printf("EOF\n");
  }
  if (first_cons) {
    last_cons->cdr = mal_empty();
    return first_cons;
  } else {
    return mal_empty();
  }
}

MalType* read_atom(Reader* reader) {
  char *token = reader_next(reader);
  if (isdigit(*token) || (strlen(token) > 1 && (*token == '-' || *token == '+') && isdigit(*(token + 1)))) {
    return mal_number(strtoll(token, NULL, 10));
  } else if (strcmp("nil", token) == 0) {
    return mal_nil();
  } else if (strcmp("true", token) == 0) {
    return mal_true();
  } else if (strcmp("false", token) == 0) {
    return mal_false();
  } else {
    return mal_symbol(token);
  }
}
