#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "types.h"

// State

MalVal *mal_error = NULL;


// Constant atomic values

MalVal mal_nil = {MAL_NIL, NULL, {0}, 0};
MalVal mal_true = {MAL_TRUE, NULL, {0}, 0};
MalVal mal_false = {MAL_FALSE, NULL, {0}, 0};


// Pre-declarations

MalVal *cons(MalVal *x, MalVal *seq);

// General Functions

// Print a hash table
#include <glib-object.h>
void g_hash_table_print(GHashTable *hash_table) {
    GHashTableIter iter;
    gpointer key, value;

    g_hash_table_iter_init (&iter, hash_table);
    while (g_hash_table_iter_next (&iter, &key, &value)) {
        g_print ("%s/%p ", (const char *) key, (void *) value);
        //g_print ("%s ", (const char *) key);
    }
}

GHashTable *g_hash_table_copy(GHashTable *src_table) {
    GHashTable *new_table = g_hash_table_new(g_str_hash, g_str_equal);
    GHashTableIter iter;
    gpointer key, value;

    g_hash_table_iter_init (&iter, src_table);
    while (g_hash_table_iter_next (&iter, &key, &value)) {
        g_hash_table_insert(new_table, key, value);
    }
    return new_table;
}

int min(int a, int b) { return a < b ? a : b; }
int max(int a, int b) { return a > b ? a : b; }

int _count(MalVal *obj) {
    switch (obj->type) {
    case MAL_NIL:      return 0;
    case MAL_LIST:     return obj->val.array->len;
    case MAL_VECTOR:   return obj->val.array->len;
    case MAL_HASH_MAP: return g_hash_table_size(obj->val.hash_table);
    case MAL_STRING:   return strlen(obj->val.string);
    default:
        _error("count unsupported for type %d\n", obj->type);
        return 0;
    }
}

// Allocate a malval and set its type and value
MalVal *malval_new(MalType type, MalVal *metadata) {
    MalVal *mv = (MalVal*)malloc(sizeof(MalVal));
    mv->type = type;
    mv->metadata = metadata;
    return mv;
}

// 
int malval_free(MalVal *mv) {
    // TODO: free collection items
    if (!(mv->type & (MAL_NIL|MAL_TRUE|MAL_FALSE))) {
        free(mv);
    }
}

MalVal *malval_new_integer(gint64 val) {
    MalVal *mv = malval_new(MAL_INTEGER, NULL);
    mv->val.intnum = val;
    return mv;
}

MalVal *malval_new_float(gdouble val) {
    MalVal *mv = malval_new(MAL_FLOAT, NULL);
    mv->val.floatnum = val;
    return mv;
}

MalVal *malval_new_string(char *val) {
    MalVal *mv = malval_new(MAL_STRING, NULL);
    mv->val.string = val;
    return mv;
}

MalVal *malval_new_symbol(char *val) {
    MalVal *mv = malval_new(MAL_SYMBOL, NULL);
    mv->val.string = val;
    return mv;
}

MalVal *malval_new_hash_map(GHashTable *val) {
    MalVal *mv = malval_new(MAL_HASH_MAP, NULL);
    mv->val.hash_table = val;
    return mv;
}

MalVal *malval_new_list(MalType type, GArray *val) {
    MalVal *mv = malval_new(type, NULL);
    mv->val.array = val;
    return mv;
}

MalVal *malval_new_atom(MalVal *val) {
    MalVal *mv = malval_new(MAL_ATOM, NULL);
    mv->val.atom_val = val;
    return mv;
}


MalVal *malval_new_function(void *(*func)(void *), int arg_cnt, MalVal* metadata) {
    MalVal *mv = malval_new(MAL_FUNCTION_C, metadata);
    mv->func_arg_cnt = arg_cnt;
    assert(mv->func_arg_cnt <= 20,
            "native function restricted to 20 args (%d given)",
            mv->func_arg_cnt);
    mv->ismacro = FALSE;
    switch (arg_cnt) {
    case -1: mv->val.f1  = (void *(*)(void*))func; break;
    case 0:  mv->val.f0  = (void *(*)())func; break;
    case 1:  mv->val.f1  = (void *(*)(void*))func; break;
    case 2:  mv->val.f2  = (void *(*)(void*,void*))func; break;
    case 3:  mv->val.f3  = (void *(*)(void*,void*,void*))func; break;
    case 4:  mv->val.f4  = (void *(*)(void*,void*,void*,void*))func; break;
    case 5:  mv->val.f5  = (void *(*)(void*,void*,void*,void*,void*))func; break;
    case 6:  mv->val.f6  = (void *(*)(void*,void*,void*,void*,void*,
                                      void*))func; break;
    case 7:  mv->val.f7  = (void *(*)(void*,void*,void*,void*,void*,
                                      void*,void*))func; break;
    case 8:  mv->val.f8  = (void *(*)(void*,void*,void*,void*,void*,
                                      void*,void*,void*))func; break;
    case 9:  mv->val.f9  = (void *(*)(void*,void*,void*,void*,void*,
                                       void*,void*,void*,void*))func; break;
    case 10: mv->val.f10 = (void *(*)(void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*,void*))func; break;
    case 11: mv->val.f11 = (void *(*)(void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*,void*,
                                      void*))func; break;
    case 12: mv->val.f12 = (void *(*)(void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*,void*,
                                      void*,void*))func; break;
    case 13: mv->val.f13 = (void *(*)(void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*,void*,
                                      void*,void*,void*))func; break;
    case 14: mv->val.f14 = (void *(*)(void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*))func; break;
    case 15: mv->val.f15 = (void *(*)(void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*,void*))func; break;
    case 16: mv->val.f16 = (void *(*)(void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*,void*,
                                      void*))func; break;
    case 17: mv->val.f17 = (void *(*)(void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*,void*,
                                      void*,void*))func; break;
    case 18: mv->val.f18 = (void *(*)(void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*,void*,
                                      void*,void*,void*))func; break;
    case 19: mv->val.f19 = (void *(*)(void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*))func; break;
    case 20: mv->val.f20 = (void *(*)(void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*,void*,
                                      void*,void*,void*,void*,void*))func; break;
    }
    return mv;
}

MalVal *apply(MalVal *f, MalVal *args) {
    MalVal *res;
    assert_type(f, MAL_FUNCTION_C|MAL_FUNCTION_MAL,
                "Cannot invoke %s", _pr_str(f,1));
    if (f->type & MAL_FUNCTION_MAL) {
        Env *fn_env = new_env(f->val.func.env, f->val.func.args, args);
        res = f->val.func.evaluator(f->val.func.body, fn_env);
        return res;
    } else {
        MalVal *a = args;
        assert((f->func_arg_cnt == -1) ||
               (f->func_arg_cnt == _count(args)),
               "Length of formal params (%d) does not match actual parameters (%d)",
               f->func_arg_cnt, _count(args));
        switch (f->func_arg_cnt) {
        case -1: res=f->val.f1 (a); break;
        case 0:  res=f->val.f0 (); break;
        case 1:  res=f->val.f1 (_nth(a,0)); break;
        case 2:  res=f->val.f2 (_nth(a,0),_nth(a,1)); break;
        case 3:  res=f->val.f3 (_nth(a,0),_nth(a,1),_nth(a,2)); break;
        case 4:  res=f->val.f4 (_nth(a,0),_nth(a,1),_nth(a,2),_nth(a,3)); break;
        case 5:  res=f->val.f5 (_nth(a,0),_nth(a,1),_nth(a,2),_nth(a,3),_nth(a,4)); break;
        case 6:  res=f->val.f6 (_nth(a,0),_nth(a,1),_nth(a,2),_nth(a,3),_nth(a,4),
                                _nth(a,5)); break;
        case 7:  res=f->val.f7 (_nth(a,0),_nth(a,1),_nth(a,2),_nth(a,3),_nth(a,4),
                                _nth(a,5),_nth(a,6)); break;
        case 8:  res=f->val.f8 (_nth(a,0),_nth(a,1),_nth(a,2),_nth(a,3),_nth(a,4),
                                _nth(a,5),_nth(a,6),_nth(a,7)); break;
        case 9:  res=f->val.f9 (_nth(a,0),_nth(a,1),_nth(a,2),_nth(a,3),_nth(a,4),
                                _nth(a,5),_nth(a,6),_nth(a,7),_nth(a,8)); break;
        case 10: res=f->val.f10(_nth(a,0),_nth(a,1),_nth(a,2),_nth(a,3),_nth(a,4),
                                _nth(a,5),_nth(a,6),_nth(a,7),_nth(a,8),_nth(a,9)); break;
        case 11: res=f->val.f11(_nth(a,0),_nth(a,1),_nth(a,2),_nth(a,3),_nth(a,4),
                                _nth(a,5),_nth(a,6),_nth(a,7),_nth(a,8),_nth(a,9),
                                _nth(a,10)); break;
        case 12: res=f->val.f12(_nth(a,0),_nth(a,1),_nth(a,2),_nth(a,3),_nth(a,4),
                                _nth(a,5),_nth(a,6),_nth(a,7),_nth(a,8),_nth(a,9),
                                _nth(a,10),_nth(a,11)); break;
        case 13: res=f->val.f13(_nth(a,0),_nth(a,1),_nth(a,2),_nth(a,3),_nth(a,4),
                                _nth(a,5),_nth(a,6),_nth(a,7),_nth(a,8),_nth(a,9),
                                _nth(a,10),_nth(a,11),_nth(a,12)); break;
        case 14: res=f->val.f14(_nth(a,0),_nth(a,1),_nth(a,2),_nth(a,3),_nth(a,4),
                                _nth(a,5),_nth(a,6),_nth(a,7),_nth(a,8),_nth(a,9),
                                _nth(a,10),_nth(a,11),_nth(a,12),_nth(a,13)); break;
        case 15: res=f->val.f15(_nth(a,0),_nth(a,1),_nth(a,2),_nth(a,3),_nth(a,4),
                                _nth(a,5),_nth(a,6),_nth(a,7),_nth(a,8),_nth(a,9),
                                _nth(a,10),_nth(a,11),_nth(a,12),_nth(a,13),_nth(a,14)); break;
        case 16: res=f->val.f16(_nth(a,0),_nth(a,1),_nth(a,2),_nth(a,3),_nth(a,4),
                                _nth(a,5),_nth(a,6),_nth(a,7),_nth(a,8),_nth(a,9),
                                _nth(a,10),_nth(a,11),_nth(a,12),_nth(a,13),_nth(a,14),
                                _nth(a,15)); break;
        case 17: res=f->val.f17(_nth(a,0),_nth(a,1),_nth(a,2),_nth(a,3),_nth(a,4),
                                _nth(a,5),_nth(a,6),_nth(a,7),_nth(a,8),_nth(a,9),
                                _nth(a,10),_nth(a,11),_nth(a,12),_nth(a,13),_nth(a,14),
                                _nth(a,15),_nth(a,16)); break;
        case 18: res=f->val.f18(_nth(a,0),_nth(a,1),_nth(a,2),_nth(a,3),_nth(a,4),
                                _nth(a,5),_nth(a,6),_nth(a,7),_nth(a,8),_nth(a,9),
                                _nth(a,10),_nth(a,11),_nth(a,12),_nth(a,13),_nth(a,14),
                                _nth(a,15),_nth(a,16),_nth(a,17)); break;
        case 19: res=f->val.f19(_nth(a,0),_nth(a,1),_nth(a,2),_nth(a,3),_nth(a,4),
                                _nth(a,5),_nth(a,6),_nth(a,7),_nth(a,8),_nth(a,9),
                                _nth(a,10),_nth(a,11),_nth(a,12),_nth(a,13),_nth(a,14),
                                _nth(a,15),_nth(a,16),_nth(a,17),_nth(a,18)); break;
        case 20: res=f->val.f20(_nth(a,0),_nth(a,1),_nth(a,2),_nth(a,3),_nth(a,4),
                                _nth(a,5),_nth(a,6),_nth(a,7),_nth(a,8),_nth(a,9),
                                _nth(a,10),_nth(a,11),_nth(a,12),_nth(a,13),_nth(a,14),
                                _nth(a,15),_nth(a,16),_nth(a,17),_nth(a,18),_nth(a,19)); break;
        }
        return res;
    }
}


char *_pr_str_hash_map(MalVal *obj, int print_readably) {
    int start = 1;
    char *repr = NULL, *repr_tmp1 = NULL, *repr_tmp2 = NULL;
    GHashTableIter iter;
    gpointer key, value;

    repr = g_strdup_printf("{");

    g_hash_table_iter_init (&iter, obj->val.hash_table);
    while (g_hash_table_iter_next (&iter, &key, &value)) {
        //g_print ("%s/%p ", (const char *) key, (void *) value);

        repr_tmp1 = _pr_str((MalVal*)value, print_readably);
        if (start) {
            start = 0;
            repr = g_strdup_printf("{\"%s\" %s", (char *)key, repr_tmp1);
        } else {
            repr_tmp2 = repr;
            repr = g_strdup_printf("%s \"%s\" %s", repr_tmp2, (char *)key, repr_tmp1);
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
        if (print_readably) {
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
    char *repr = g_strdup_printf(""),
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

// Return a string representation of a MalVal sequence (in a format that can
// be read by the reader). Returned string must be freed by caller.
MalVal *pr_str(MalVal *args) {
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "pr_str called with non-sequential args");
    return malval_new_string(_pr_str_args(args, " ", 1));
}

// Return a string representation of a MalVal sequence with every item
// concatenated together. Returned string must be freed by caller.
MalVal *str(MalVal *args) {
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "str called with non-sequential args");
    return malval_new_string(_pr_str_args(args, "", 0));
}

// Print a string representation of a MalVal sequence (in a format that can
// be read by the reader) followed by a newline. Returns nil.
MalVal *prn(MalVal *args) {
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "prn called with non-sequential args");
    char *repr = _pr_str_args(args, " ", 1);
    g_print("%s\n", repr);
    free(repr);
    return &mal_nil;
}

// Print a string representation of a MalVal sequence (for human consumption)
// followed by a newline. Returns nil.
MalVal *println(MalVal *args) {
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "println called with non-sequential args");
    char *repr = _pr_str_args(args, " ", 0);
    g_print("%s\n", repr);
    free(repr);
    return &mal_nil;
}

MalVal *with_meta(MalVal *obj, MalVal *meta) {
    MalVal *new_obj = malval_new(obj->type, meta);
    new_obj->val = obj->val;
    return new_obj;
}

MalVal *meta(MalVal *obj) {
    assert_type(obj, MAL_LIST|MAL_VECTOR|MAL_HASH_MAP|MAL_FUNCTION_C|MAL_FUNCTION_MAL,
                "attempt to get metadata from non-collection type");
    if (obj->metadata == NULL) {
        return &mal_nil;
    } else {
        return obj->metadata;
    }
}


int _equal_Q(MalVal *a, MalVal *b) {
    if (a == NULL || b == NULL) { return FALSE; }

    // If types are the same or both are sequential then they might be equal
    if (!((a->type == b->type) ||
          (_sequential_Q(a) && _sequential_Q(b)))) {
        return FALSE;
    }
    switch (a->type) {
    case MAL_NIL:
    case MAL_TRUE:
    case MAL_FALSE:
        return a->type == b->type;
    case MAL_INTEGER:
        return a->val.intnum == b->val.intnum;
    case MAL_FLOAT:
        return a->val.floatnum == b->val.floatnum;
    case MAL_SYMBOL:
    case MAL_STRING:
        if (strcmp(a->val.string, b->val.string) == 0) {
            return TRUE;
        } else {
            return FALSE;
        }
    case MAL_LIST:
    case MAL_VECTOR:
        if (a->val.array->len != b->val.array->len) {
            return FALSE;
        }
        int i;
        for (i=0; i<a->val.array->len; i++) {
            if (! _equal_Q(g_array_index(a->val.array, MalVal*, i),
                           g_array_index(b->val.array, MalVal*, i))) {
                return FALSE;
            }
        }
        return TRUE;
    case MAL_HASH_MAP:
        _error("_equal_Q does not support hash-maps yet");
        return FALSE;
    case MAL_FUNCTION_C:
    case MAL_FUNCTION_MAL:
        return a->val.f0 == b->val.f0;
    default:
        _error("_equal_Q unsupported comparison type %d\n", a->type);
        return FALSE;
    }
}

MalVal *equal_Q(MalVal *a, MalVal *b) {
    if (_equal_Q(a, b)) { return &mal_true; }
    else { return &mal_false; }
}

//
// nil, true, false, string
MalVal *nil_Q(MalVal *seq) { return seq->type & MAL_NIL ? &mal_true : &mal_false; }
MalVal *true_Q(MalVal *seq) { return seq->type & MAL_TRUE ? &mal_true : &mal_false; }
MalVal *false_Q(MalVal *seq) { return seq->type & MAL_FALSE ? &mal_true : &mal_false; }
MalVal *string_Q(MalVal *seq) { return seq->type & MAL_STRING ? &mal_true : &mal_false; }

//
// Numbers
#define WRAP_INTEGER_OP(name, op) \
    MalVal *int_ ## name(MalVal *a, MalVal *b)     { \
        return malval_new_integer(a->val.intnum op b->val.intnum); \
    }
#define WRAP_INTEGER_CMP_OP(name, op) \
    MalVal *int_ ## name(MalVal *a, MalVal *b)     { \
        return a->val.intnum op b->val.intnum ? &mal_true : &mal_false; \
    }
WRAP_INTEGER_OP(plus,+)
WRAP_INTEGER_OP(minus,-)
WRAP_INTEGER_OP(multiply,*)
WRAP_INTEGER_OP(divide,/)
WRAP_INTEGER_CMP_OP(gt,>)
WRAP_INTEGER_CMP_OP(gte,>=)
WRAP_INTEGER_CMP_OP(lt,<)
WRAP_INTEGER_CMP_OP(lte,<=)


//
// Symbols
MalVal *symbol_Q(MalVal *seq) { return seq->type & MAL_SYMBOL ? &mal_true : &mal_false; }


// Hash maps
//
MalVal *_hash_map(int count, ...) {
    assert((count % 2) == 0,
           "odd number of parameters to hash-map");
    GHashTable *htable = g_hash_table_new(g_str_hash, g_str_equal);
    MalVal *hm = malval_new_hash_map(htable);
    char *k;
    MalVal *v;
    va_list ap;
    va_start(ap, count);
    while (count > 0) {
        k = va_arg(ap, char*);
        v = va_arg(ap, MalVal*);
        g_hash_table_insert(htable, k, v);
        count = count - 2;
    }
    va_end(ap);
    return hm;
}

MalVal *_assoc_BANG(MalVal* hm, MalVal *args) {
    assert((_count(args) % 2) == 0,
           "odd number of parameters to assoc!");
    GHashTable *htable = hm->val.hash_table;
    int i;
    MalVal *k, *v;
    for (i=0; i<_count(args); i+=2) {
        k = g_array_index(args->val.array, MalVal*, i);
        assert_type(k, MAL_STRING,
                    "assoc! called with non-string key");
        v = g_array_index(args->val.array, MalVal*, i+1);
        g_hash_table_insert(htable, k->val.string, v);
    }
    return hm;
}

MalVal *_dissoc_BANG(MalVal* hm, MalVal *args) {
    GHashTable *htable = hm->val.hash_table;
    int i;
    MalVal *k, *v;
    for (i=0; i<_count(args); i++) {
        k = g_array_index(args->val.array, MalVal*, i);
        assert_type(k, MAL_STRING,
                    "dissoc! called with non-string key");
        g_hash_table_remove(htable, k->val.string);
    }
    return hm;
}

MalVal *hash_map(MalVal *args) {
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "hash-map called with non-sequential arguments");
    GHashTable *htable = g_hash_table_new(g_str_hash, g_str_equal);
    MalVal *hm = malval_new_hash_map(htable);
    return _assoc_BANG(hm, args);
}

int _hash_map_Q(MalVal *seq) {
    return seq->type & MAL_HASH_MAP;
}
MalVal *hash_map_Q(MalVal *seq) { return _hash_map_Q(seq) ? &mal_true : &mal_false; }

MalVal *assoc(MalVal *args) {
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "assoc called with non-sequential arguments");
    assert(_count(args) >= 2,
           "assoc needs at least 2 arguments");
    GHashTable *htable = g_hash_table_copy(_nth(args,0)->val.hash_table);
    MalVal *hm = malval_new_hash_map(htable);
    return _assoc_BANG(hm, rest(args));
}

MalVal *dissoc(MalVal* args) {
    GHashTable *htable = g_hash_table_copy(_nth(args,0)->val.hash_table);
    MalVal *hm = malval_new_hash_map(htable);
    return _dissoc_BANG(hm, rest(args));
}

MalVal *keys(MalVal *obj) {
    assert_type(obj, MAL_HASH_MAP,
                "keys called on non-hash-map");

    GHashTableIter iter;
    gpointer key, value;
    MalVal *seq = malval_new_list(MAL_LIST,
                                  g_array_sized_new(TRUE, TRUE, sizeof(MalVal*),
                                                    _count(obj)));
    g_hash_table_iter_init (&iter, obj->val.hash_table);
    while (g_hash_table_iter_next (&iter, &key, &value)) {
        MalVal *kname = malval_new_string((char *)key);
        g_array_append_val(seq->val.array, kname);
    }
    return seq;
}

MalVal *vals(MalVal *obj) {
    assert_type(obj, MAL_HASH_MAP,
                "vals called on non-hash-map");

    GHashTableIter iter;
    gpointer key, value;
    MalVal *seq = malval_new_list(MAL_LIST,
                                  g_array_sized_new(TRUE, TRUE, sizeof(MalVal*),
                                                    _count(obj)));
    g_hash_table_iter_init (&iter, obj->val.hash_table);
    while (g_hash_table_iter_next (&iter, &key, &value)) {
        g_array_append_val(seq->val.array, value);
    }
    return seq;
}


// Errors/Exceptions
void _error(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    mal_error = malval_new_string(g_strdup_vprintf(fmt, args));
}
void throw(MalVal *obj) {
    mal_error = obj;
}


// Lists

MalVal *_list(int count, ...) {
    MalVal *seq = malval_new_list(MAL_LIST,
                                  g_array_sized_new(TRUE, TRUE, sizeof(MalVal*),
                                                    count));
    MalVal *v;
    va_list ap;
    va_start(ap, count);
    while (count-- > 0) {
        v = va_arg(ap, MalVal*);
        g_array_append_val(seq->val.array, v);
    }
    va_end(ap);
    return seq;
}
MalVal *list(MalVal *args) {
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "list called with invalid arguments");
    args->type = MAL_LIST;
    return args;
}

int _list_Q(MalVal *seq) {
    return seq->type & MAL_LIST;
}
MalVal *list_Q(MalVal *seq) { return _list_Q(seq) ? &mal_true : &mal_false; }


// Vectors

MalVal *_vector(int count, ...) {
    MalVal *seq = malval_new_list(MAL_VECTOR,
                                  g_array_sized_new(TRUE, TRUE, sizeof(MalVal*),
                                                    count));
    MalVal *v;
    va_list ap;
    va_start(ap, count);
    while (count-- > 0) {
        v = va_arg(ap, MalVal*);
        g_array_append_val(seq->val.array, v);
    }
    va_end(ap);
    return seq;
}
MalVal *vector(MalVal *args) {
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "vector called with invalid arguments");
    args->type = MAL_VECTOR;
    return args;
}


int _vector_Q(MalVal *seq) {
    return seq->type & MAL_VECTOR;
}
MalVal *vector_Q(MalVal *seq) { return _vector_Q(seq) ? &mal_true : &mal_false; }


// hash map and vector functions
MalVal *get(MalVal *obj, MalVal *key) {
    MalVal *val;
    switch (obj->type) {
    case MAL_VECTOR:
        return _nth(obj, key->val.intnum);
    case MAL_HASH_MAP:
        if (g_hash_table_lookup_extended(obj->val.hash_table,
                                         key->val.string,
                                         NULL, (gpointer*)&val)) {
            return val;
        } else {
            return &mal_nil;
        }
    default:
        abort("get called on unsupported type %d", obj->type);
    }
}

MalVal *contains_Q(MalVal *obj, MalVal *key) {
    switch (obj->type) {
    case MAL_VECTOR:
        if (key->val.intnum < obj->val.array->len) {
            return &mal_true;
        } else {
            return &mal_false;
        }
    case MAL_HASH_MAP:
        if (g_hash_table_contains(obj->val.hash_table, key->val.string)) {
            return &mal_true;
        } else {
            return &mal_false;
        }
    default:
        abort("contains? called on unsupported type %d", obj->type);
    }
}


// Atoms
MalVal *atom(MalVal *val) {
    return malval_new_atom(val);
}

int _atom_Q(MalVal *exp) {
    return exp->type & MAL_ATOM;
}
MalVal *atom_Q(MalVal *exp) { return _atom_Q(exp) ? &mal_true : &mal_false; }

MalVal *deref(MalVal *atm) {
    assert_type(atm, MAL_ATOM,
                "deref called on non-atom");
    return atm->val.atom_val;
}

MalVal *reset_BANG(MalVal *atm, MalVal *val) {
    assert_type(atm, MAL_ATOM,
                "reset! called with non-atom");
    atm->val.atom_val = val;
    return val;
}

MalVal *swap_BANG(MalVal *args) {
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "swap! called with invalid arguments");
    assert(_count(args) >= 2,
           "swap! called with %d args, needs at least 2", _count(args));
    MalVal *atm = _nth(args, 0),
           *f = _nth(args, 1),
           *sargs = _slice(args, 2, _count(args)),
           *fargs = cons(atm->val.atom_val, sargs),
           *new_val = apply(f, fargs);
    if (mal_error) { return NULL; }
    atm->val.atom_val = new_val;
    return new_val;
}



// Sequence functions
MalVal *_slice(MalVal *seq, int start, int end) {
    int i, new_len = max(0, min(end-start,
                                _count(seq)-start));
    GArray *new_arr = g_array_sized_new(TRUE, TRUE, sizeof(MalVal*),
                                        new_len);
    for (i=start; i<start+new_len; i++) {
        g_array_append_val(new_arr, g_array_index(seq->val.array, MalVal*, i));
    }
    return malval_new_list(MAL_LIST, new_arr);
}


int _sequential_Q(MalVal *seq) {
    return seq->type & (MAL_LIST|MAL_VECTOR);
}
MalVal *sequential_Q(MalVal *seq) {
    return _sequential_Q(seq) ? &mal_true : &mal_false;
}

MalVal *cons(MalVal *x, MalVal *seq) {
    assert_type(seq, MAL_LIST|MAL_VECTOR,
                "second argument to cons is non-sequential");
    int i, len = _count(seq);
    GArray *new_arr = g_array_sized_new(TRUE, TRUE, sizeof(MalVal*),
                                        len+1);
    g_array_append_val(new_arr, x);
    for (i=0; i<len; i++) {
        g_array_append_val(new_arr, g_array_index(seq->val.array, MalVal*, i));
    }
    return malval_new_list(MAL_LIST, new_arr);
}

MalVal *count(MalVal *seq) {
    return malval_new_integer(_count(seq));
}

MalVal *empty_Q(MalVal *seq) {
    assert_type(seq, MAL_LIST|MAL_VECTOR,
                "empty? called with non-sequential");
    return (seq->val.array->len == 0) ? &mal_true : &mal_false;
}

MalVal *concat(MalVal *args) {
    MalVal *arg, *e, *lst;
    int i, j, arg_cnt = _count(args);
    lst = malval_new_list(MAL_LIST,
                        g_array_sized_new(TRUE, TRUE, sizeof(MalVal*), arg_cnt));
    for (i=0; i<arg_cnt; i++) {
        arg = g_array_index(args->val.array, MalVal*, i);
        assert_type(arg, MAL_LIST|MAL_VECTOR,
                    "concat called with non-sequential");
        for (j=0; j<_count(arg); j++) {
            e = g_array_index(arg->val.array, MalVal*, j);
            g_array_append_val(lst->val.array, e);
        }
    }

    return lst;
}

MalVal *sconj(MalVal *args) {
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "conj called with non-sequential");
    MalVal *src_lst = _nth(args, 0);
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "first argument to conj is non-sequential");
    int i, len = _count(src_lst) + _count(args) - 1;
    GArray *new_arr = g_array_sized_new(TRUE, TRUE, sizeof(MalVal*),
                                        len);
    // Copy in src_lst
    for (i=0; i<_count(src_lst); i++) {
        g_array_append_val(new_arr, g_array_index(src_lst->val.array, MalVal*, i));
    }
    // Conj extra args
    for (i=1; i<_count(args); i++) {
        if (src_lst->type & MAL_LIST) {
            g_array_prepend_val(new_arr, g_array_index(args->val.array, MalVal*, i));
        } else {
            g_array_append_val(new_arr, g_array_index(args->val.array, MalVal*, i));
        }
    }
    return malval_new_list(src_lst->type, new_arr);
}

MalVal *first(MalVal *seq) {
    assert_type(seq, MAL_LIST|MAL_VECTOR,
                "first called with non-sequential");
    if (_count(seq) == 0) {
        return &mal_nil;
    }
    return g_array_index(seq->val.array, MalVal*, 0);
}

MalVal *last(MalVal *seq) {
    assert_type(seq, MAL_LIST|MAL_VECTOR,
                "last called with non-sequential");
    if (_count(seq) == 0) {
        return &mal_nil;
    }
    return g_array_index(seq->val.array, MalVal*, _count(seq)-1);
}

MalVal *rest(MalVal *seq) {
    return _slice(seq, 1, _count(seq));
}

MalVal *_nth(MalVal *seq, int idx) {
    assert_type(seq, MAL_LIST|MAL_VECTOR,
                "nth called with non-sequential");
    if (idx >= _count(seq)) {
        return &mal_nil;
    }
    return g_array_index(seq->val.array, MalVal*, idx);
}
MalVal *nth(MalVal *seq, MalVal *idx) {
    return _nth(seq, idx->val.intnum);
}

MalVal *sapply(MalVal *args) {
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "apply called with non-sequential");
    MalVal *f = _nth(args, 0);
    MalVal *last_arg = _nth(args, _count(args)-1); 
    assert_type(last_arg, MAL_LIST|MAL_VECTOR,
                "last argument to apply is non-sequential");
    int i, len = _count(args) - 2 + _count(last_arg);
    GArray *new_arr = g_array_sized_new(TRUE, TRUE, sizeof(MalVal*),
                                        len);
    // Initial arguments
    for (i=1; i<_count(args)-1; i++) {
        g_array_append_val(new_arr, g_array_index(args->val.array, MalVal*, i));
    }
    // Add arguments from last_arg
    for (i=0; i<_count(last_arg); i++) {
        g_array_append_val(new_arr, g_array_index(last_arg->val.array, MalVal*, i));
    }
    return apply(f, malval_new_list(MAL_LIST, new_arr));
}

MalVal *_map2(MalVal *(*func)(void*, void*), MalVal *lst, void *arg2) {
    MalVal *e, *el;
    assert_type(lst, MAL_LIST|MAL_VECTOR,
                "_map called with non-sequential");
    int i, len = _count(lst);
    el = malval_new_list(MAL_LIST,
                         g_array_sized_new(TRUE, TRUE, sizeof(MalVal*), len));
    for (i=0; i<len; i++) {
        e = func(g_array_index(lst->val.array, MalVal*, i), arg2);
        if (!e || mal_error) return NULL;
        g_array_append_val(el->val.array, e);
    }
    return el;
}

MalVal *map(MalVal *mvf, MalVal *lst) {
    MalVal *res, *el;
    assert_type(mvf, MAL_FUNCTION_C|MAL_FUNCTION_MAL,
                "map called with non-function");
    assert_type(lst, MAL_LIST|MAL_VECTOR,
                "map called with non-sequential");
    int i, len = _count(lst);
    el = malval_new_list(MAL_LIST,
                         g_array_sized_new(TRUE, TRUE, sizeof(MalVal*), len));
    for (i=0; i<len; i++) {
        // TODO: this is replicating some of apply functionality
        if (mvf->type & MAL_FUNCTION_MAL) {
            Env *fn_env = new_env(mvf->val.func.env,
                                  mvf->val.func.args,
                                  _slice(lst, i, i+1));
            res = mvf->val.func.evaluator(mvf->val.func.body, fn_env);
        } else {
            res = mvf->val.f1(g_array_index(lst->val.array, MalVal*, i));
        }
        if (!res || mal_error) return NULL;
        g_array_append_val(el->val.array, res);
    }
    return el;
}


// Env

Env *new_env(Env *outer, MalVal* binds, MalVal *exprs) {
    Env *e = malloc(sizeof(Env));
    e->table = g_hash_table_new(g_str_hash, g_str_equal);
    e->outer = outer;

    if (binds && exprs) {
        assert_type(binds, MAL_LIST|MAL_VECTOR,
                    "new_env called with non-sequential bindings");
        assert_type(exprs, MAL_LIST|MAL_VECTOR,
                    "new_env called with non-sequential expressions");
        int binds_len = _count(binds),
            exprs_len = _count(exprs),
            varargs = 0, i;
        for (i=0; i<binds_len; i++) {
            if (i > exprs_len) { break; }
            if (_nth(binds, i)->val.string[0] == '&') {
                varargs = 1;
                env_set(e, _nth(binds, i+1)->val.string, _slice(exprs, i, _count(exprs)));
                break;
            } else {
              env_set(e, _nth(binds, i)->val.string, _nth(exprs, i));
            }
        }
        assert(varargs || (binds_len == exprs_len),
               "Arity mismatch: %d formal params vs %d actual params",
               binds_len, exprs_len);

    }
    return e;
}

Env *env_find(Env *env, char *key) {
    void *val = g_hash_table_lookup(env->table, key);
    if (val) {
        return env;
    } else if (env->outer) {
        return env_find(env->outer, key);
    } else {
        return NULL;
    }
}

MalVal *env_get(Env *env, char *key) {
    Env *e = env_find(env, key);
    assert(e, "'%s' not found", key);
    return g_hash_table_lookup(e->table, key);
}

Env *env_set(Env *env, char *key, MalVal *val) {
    g_hash_table_insert(env->table, key, val);
    return env;
}

types_ns_entry types_ns[49] = {
    {"pr-str", (void*(*)(void*))pr_str, -1},
    {"str", (void*(*)(void*))str, -1},
    {"prn", (void*(*)(void*))prn, -1},
    {"println", (void*(*)(void*))println, -1},
    {"with-meta", (void*(*)(void*))with_meta, 2},
    {"meta", (void*(*)(void*))meta, 1},
    {"=", (void*(*)(void*))equal_Q, 2},
    {"symbol?", (void*(*)(void*))symbol_Q, 1},
    {"nil?", (void*(*)(void*))nil_Q, 1},
    {"true?", (void*(*)(void*))true_Q, 1},
    {"false?", (void*(*)(void*))false_Q, 1},
    {"+", (void*(*)(void*))int_plus, 2},
    {"-", (void*(*)(void*))int_minus, 2},
    {"*", (void*(*)(void*))int_multiply, 2},
    {"/", (void*(*)(void*))int_divide, 2},
    {">", (void*(*)(void*))int_gt, 2},
    {">=", (void*(*)(void*))int_gte, 2},
    {"<", (void*(*)(void*))int_lt, 2},
    {"<=", (void*(*)(void*))int_lte, 2},
    {"hash-map", (void*(*)(void*))hash_map, -1},
    {"map?", (void*(*)(void*))hash_map_Q, 1},
    {"assoc", (void*(*)(void*))assoc, -1},
    {"dissoc", (void*(*)(void*))dissoc, -1},
    {"get", (void*(*)(void*))get, 2},
    {"contains?", (void*(*)(void*))contains_Q, 2},
    {"keys", (void*(*)(void*))keys, 1},
    {"vals", (void*(*)(void*))vals, 1},
    {"throw", (void*(*)(void*))throw, 1},
    {"list", (void*(*)(void*))list, -1},
    {"list?", (void*(*)(void*))list_Q, 1},
    {"vector", (void*(*)(void*))vector, -1},
    {"vector?", (void*(*)(void*))vector_Q, 1},
    {"atom", (void*(*)(void*))atom, 1},
    {"atom?", (void*(*)(void*))atom_Q, 1},
    {"deref", (void*(*)(void*))deref, 1},
    {"reset!", (void*(*)(void*))reset_BANG, 2},
    {"swap!", (void*(*)(void*))swap_BANG, -1},
    {"sequential?", (void*(*)(void*))sequential_Q, 1},
    {"cons", (void*(*)(void*))cons, 2},
    {"count", (void*(*)(void*))count, 1},
    {"empty?", (void*(*)(void*))empty_Q, 1},
    {"concat", (void*(*)(void*))concat, -1},
    {"conj", (void*(*)(void*))sconj, -1},
    {"first", (void*(*)(void*))first, 1},
    {"last", (void*(*)(void*))last, 1},
    {"rest", (void*(*)(void*))rest, 1},
    {"nth", (void*(*)(void*))nth, 2},
    {"apply", (void*(*)(void*))sapply, -1},
    {"map", (void*(*)(void*))map, 2},
    };
