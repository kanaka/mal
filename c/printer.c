#include <stdlib.h>
#include <stdio.h>
#include "types.h"
#include "printer.h"

char *_pr_str_hash_map(MalVal *obj, int print_readably) {
    int start = 1;
    char *repr = NULL, *repr_tmp1 = NULL, *repr_tmp2 = NULL,
         *key2 = NULL;
    GHashTableIter iter;
    gpointer key, value;

    repr = g_strdup_printf("{");

    g_hash_table_iter_init (&iter, obj->val.hash_table);
    while (g_hash_table_iter_next (&iter, &key, &value)) {
        //g_print ("%s/%p ", (const char *) key, (void *) value);
        if (((char*)key)[0] == '\x7f') {
            key2 = g_strdup_printf("%s", (char*)key);
            key2[0] = ':';
        } else {
            key2 = g_strdup_printf("\"%s\"", (char*)key);
        }

        repr_tmp1 = _pr_str((MalVal*)value, print_readably);
        if (start) {
            start = 0;
            repr = g_strdup_printf("{%s %s", (char*)key2, repr_tmp1);
        } else {
            repr_tmp2 = repr;
            repr = g_strdup_printf("%s %s %s", repr_tmp2, (char*)key2, repr_tmp1);
            free(repr_tmp2);
        }
        free(repr_tmp1);
    }
    repr_tmp2 = repr;
    repr = g_strdup_printf("%s}", repr_tmp2);
    free(repr_tmp2);
    return repr;
}

char *_pr_str_list(MalVal *obj, int print_readably, char start, char end) {
    int i;
    char *repr = NULL, *repr_tmp1 = NULL, *repr_tmp2 = NULL;
    repr = g_strdup_printf("%c", start);
    for (i=0; i<_count(obj); i++) {
        repr_tmp1 = _pr_str(g_array_index(obj->val.array, MalVal*, i),
                            print_readably);
        if (i == 0) {
            repr = g_strdup_printf("%c%s", start, repr_tmp1);
        } else {
            repr_tmp2 = repr;
            repr = g_strdup_printf("%s %s", repr_tmp2, repr_tmp1);
            free(repr_tmp2);
        }
        free(repr_tmp1);
    }
    repr_tmp2 = repr;
    repr = g_strdup_printf("%s%c", repr_tmp2, end);
    free(repr_tmp2);
    return repr;
}

// Return a string representation of the MalVal object. Returned string must
// be freed by caller.
char *_pr_str(MalVal *obj, int print_readably) {
    char *repr = NULL;
    if (obj == NULL) { return NULL; }
    switch (obj->type) {
    case MAL_NIL:
        repr = g_strdup_printf("nil");
        break;
    case MAL_TRUE:
        repr = g_strdup_printf("true");
        break;
    case MAL_FALSE:
        repr = g_strdup_printf("false");
        break;
    case MAL_STRING:
        if (obj->val.string[0] == '\x7f') {
            // Keyword
            repr = g_strdup_printf("%s", obj->val.string);
            repr[0] = ':';
        } else if (print_readably) {
            char *repr_tmp = g_strescape(obj->val.string, "");
            repr = g_strdup_printf("\"%s\"", repr_tmp);
            free(repr_tmp);
        } else {
            repr = g_strdup_printf("%s", obj->val.string);
        }
        break;
    case MAL_SYMBOL:
        repr = g_strdup_printf("%s", obj->val.string);
        break;
    case MAL_INTEGER:
        repr = g_strdup_printf("%" G_GINT64_FORMAT, obj->val.intnum);
        break;
    case MAL_FLOAT:
        repr = g_strdup_printf("%f", obj->val.floatnum);
        break;
    case MAL_HASH_MAP:
        repr = _pr_str_hash_map(obj, print_readably);
        break;
    case MAL_LIST:
        repr = _pr_str_list(obj, print_readably, '(', ')');
        break;
    case MAL_VECTOR:
        repr = _pr_str_list(obj, print_readably, '[', ']');
        break;
    case MAL_ATOM:
        repr = g_strdup_printf("(atom %s)",
                               _pr_str(obj->val.atom_val, print_readably));
        break;
    case MAL_FUNCTION_C:
        repr = g_strdup_printf("#<function@%p>", obj->val.f0);
        break;
    case MAL_FUNCTION_MAL:
        repr = g_strdup_printf("#<Function: (fn* %s %s)>",
                                _pr_str(obj->val.func.args, print_readably),
                                _pr_str(obj->val.func.body, print_readably));
        break;
    default:
        printf("pr_str unknown type %d\n", obj->type);
        repr = g_strdup_printf("<unknown>");
    }
    return repr;
}

// Return a string representation of the MalVal arguments. Returned string must
// be freed by caller.
char *_pr_str_args(MalVal *args, char *sep, int print_readably) {
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "_pr_str called with non-sequential args");
    int i;
    char *repr = g_strdup_printf("%s", ""),
         *repr2 = NULL;
    for (i=0; i<_count(args); i++) {
        MalVal *obj = g_array_index(args->val.array, MalVal*, i);
        if (i != 0) {
            repr2 = repr;
            repr = g_strdup_printf("%s%s", repr2, sep);
            free(repr2);
        }
        repr2 = repr;
        repr = g_strdup_printf("%s%s",
                               repr2, _pr_str(obj, print_readably));
        free(repr2);
    }
    return repr;
}

