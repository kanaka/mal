#include <stdlib.h>
#include <stdio.h>
#include <string.h>

//#include <glib/gregex.h>
//#include <glib-object.h>
#include <glib.h>

#include "types.h"
#include "reader.h"

// Declare
MalVal *read_form(Reader *reader);

Reader *reader_new() {
    Reader *reader = (Reader*)malloc(sizeof(Reader));
    reader->array = g_array_sized_new(TRUE, FALSE, sizeof(char *), 8);
    reader->position = 0;
    return reader;
}

int reader_append(Reader *reader, char* token) {
    g_array_append_val(reader->array, token);
    return TRUE;
}

char *reader_peek(Reader *reader) {
    return g_array_index(reader->array, char*, reader->position);
}

char *reader_next(Reader *reader) {
    if (reader->position >= reader->array->len) {
        return NULL;
    } else {
        return g_array_index(reader->array, char*, reader->position++);
    }
}

void reader_free(Reader *reader) {
    int i;
    for(i=0; i < reader->array->len; i++) {
        free(g_array_index(reader->array, char*, i));
    }
    g_array_free(reader->array, TRUE); 
    free(reader);
}

Reader *tokenize(char *line) {
    GRegex *regex;
    GMatchInfo *matchInfo;
    GError *err = NULL;

    Reader *reader = reader_new();

    regex = g_regex_new ("[\\s ,]*(~@|[\\[\\]{}()'`~@]|\"(?:[\\\\].|[^\\\\\"])*\"|;.*|[^\\s \\[\\]{}()'\"`~@,;]*)", 0, 0, &err);   
    g_regex_match (regex, line, 0, &matchInfo);

    if (err != NULL) {
        fprintf(stderr, "Tokenize error: %s\n", err->message);
        return NULL;
    }
    
    while (g_match_info_matches(matchInfo)) {
        gchar *result = g_match_info_fetch(matchInfo, 1);
        if (result[0] != '\0' && result[0] != ';') {
            reader_append(reader, result);
        }
        g_match_info_next(matchInfo, &err);
    }
    g_match_info_free(matchInfo);
    g_regex_unref(regex);
    if (reader->array->len == 0) {
        reader_free(reader);
        return NULL;
    } else {
        return reader;
    }
}


// From http://creativeandcritical.net/str-replace-c/ - Laird Shaw
char *replace_str(const char *str, const char *old, const char *new)
{
    char *ret, *r;
    const char *p, *q;
    size_t oldlen = strlen(old);
    size_t count, retlen, newlen = strlen(new);

    if (oldlen != newlen) {
        for (count = 0, p = str; (q = strstr(p, old)) != NULL; p = q + oldlen)
            count++;
        /* this is undefined if p - str > PTRDIFF_MAX */
        retlen = p - str + strlen(p) + count * (newlen - oldlen);
    } else
        retlen = strlen(str);

    if ((ret = malloc(retlen + 1)) == NULL)
        return NULL;

    for (r = ret, p = str; (q = strstr(p, old)) != NULL; p = q + oldlen) {
        /* this is undefined if q - p > PTRDIFF_MAX */
        ptrdiff_t l = q - p;
        memcpy(r, p, l);
        r += l;
        memcpy(r, new, newlen);
        r += newlen;
    }
    strcpy(r, p);

    return ret;
}


MalVal *read_atom(Reader *reader) {
    char *token;
    GRegex *regex;
    GMatchInfo *matchInfo;
    GError *err = NULL;
    gint pos;
    MalVal *atom;

    token = reader_next(reader);
    //g_print("read_atom token: %s\n", token);
    
    regex = g_regex_new ("(^-?[0-9]+$)|(^-?[0-9][0-9.]*$)|(^nil$)|(^true$)|(^false$)|^\"(.*)\"$|:(.*)|(^[^\"]*$)", 0, 0, &err);
    g_regex_match (regex, token, 0, &matchInfo);

    if (g_match_info_fetch_pos(matchInfo, 1, &pos, NULL) && pos != -1) {
        //g_print("read_atom integer\n");
        atom = malval_new_integer(g_ascii_strtoll(token, NULL, 10));
    } else if (g_match_info_fetch_pos(matchInfo, 2, &pos, NULL) && pos != -1) {
        //g_print("read_atom float\n");
        atom = malval_new_float(g_ascii_strtod(token, NULL));
    } else if (g_match_info_fetch_pos(matchInfo, 3, &pos, NULL) && pos != -1) {
        //g_print("read_atom nil\n");
        atom = &mal_nil;
    } else if (g_match_info_fetch_pos(matchInfo, 4, &pos, NULL) && pos != -1) {
        //g_print("read_atom true\n");
        atom = &mal_true;
    } else if (g_match_info_fetch_pos(matchInfo, 5, &pos, NULL) && pos != -1) {
        //g_print("read_atom false\n");
        atom = &mal_false;
    } else if (g_match_info_fetch_pos(matchInfo, 6, &pos, NULL) && pos != -1) {
        //g_print("read_atom string: %s\n", token);
        char *str_tmp = replace_str(g_match_info_fetch(matchInfo, 6), "\\\"", "\"");
        atom = malval_new_string(str_tmp);
    } else if (g_match_info_fetch_pos(matchInfo, 7, &pos, NULL) && pos != -1) {
        //g_print("read_atom keyword\n");
        atom = malval_new_keyword(g_match_info_fetch(matchInfo, 7));
    } else if (g_match_info_fetch_pos(matchInfo, 8, &pos, NULL) && pos != -1) {
        //g_print("read_atom symbol\n");
        atom = malval_new_symbol(g_match_info_fetch(matchInfo, 8));
    } else {
        malval_free(atom);
        atom = NULL;
    }
    return atom;
}

MalVal *read_list(Reader *reader, MalType type, char start, char end) {
    MalVal *ast, *form;
    char *token = reader_next(reader);
    //g_print("read_list start token: %s\n", token);
    if (token[0] != start) { abort("expected '(' or '['"); }

    ast = malval_new_list(type, g_array_new(TRUE, TRUE, sizeof(MalVal*)));

    while ((token = reader_peek(reader)) &&
           token[0] != end) {
        //g_print("read_list internal token %s\n", token);
        form = read_form(reader);
        if (!form) {
            if (!mal_error) { abort("unknown read_list failure"); }
            g_array_free(ast->val.array, TRUE);
            malval_free(ast);
            return NULL;
        }
        g_array_append_val(ast->val.array, form);
    }
    if (!token) { abort("expected ')' or ']', got EOF"); }
    reader_next(reader);
    //g_print("read_list end token: %s\n", token);
    return ast;
}

MalVal *read_hash_map(Reader *reader) {
    MalVal *lst = read_list(reader, MAL_LIST, '{', '}');
    MalVal *hm = _hash_map(lst);
    malval_free(lst);
    return hm;
}


MalVal *read_form(Reader *reader) {
    char *token;
    MalVal *form = NULL, *tmp;

//    while(token = reader_next(reader)) {
//        printf("token: %s\n", token);
//    }
//    return NULL;

    token = reader_peek(reader);

    if (!token) { return NULL; }
    //g_print("read_form token: %s\n", token);

    switch (token[0]) {
    case ';':
        abort("comments not yet implemented");
        break;
    case '\'':
        reader_next(reader);
        form = _listX(2, malval_new_symbol("quote"),
                         read_form(reader));
        break;
    case '`':
        reader_next(reader);
        form = _listX(2, malval_new_symbol("quasiquote"),
                         read_form(reader));
        break;
    case '~':
        reader_next(reader);
        if (token[1] == '@') {
            form = _listX(2, malval_new_symbol("splice-unquote"),
                             read_form(reader));
        } else {
            form = _listX(2, malval_new_symbol("unquote"),
                             read_form(reader));
        };
        break;
    case '^':
        reader_next(reader);
        MalVal *meta = read_form(reader);
        form = _listX(3, malval_new_symbol("with-meta"),
                         read_form(reader), meta);
        break;
    case '@':
        reader_next(reader);
        form = _listX(2, malval_new_symbol("deref"),
                         read_form(reader));
        break;


    // list
    case ')':
        abort("unexpected ')'");
        break;
    case '(':
        form = read_list(reader, MAL_LIST, '(', ')');
        break;

    // vector
    case ']':
        abort("unexpected ']'");
        break;
    case '[':
        form = read_list(reader, MAL_VECTOR, '[', ']');
        break;

    // hash-map
    case '}':
        abort("unexpected '}'");
        break;
    case '{':
        form = read_hash_map(reader);
        break;

    default:
        form = read_atom(reader);
        break;
    }
    return form;

}

MalVal *read_str (char *str) {
    Reader *reader;
    char *token;
    MalVal *ast = NULL;

    reader = tokenize(str);
    if (reader) {
        ast = read_form(reader);
        reader_free(reader);
    }

    return ast;
}
