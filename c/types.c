#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "types.h"
#include "printer.h"


// Errors/Exceptions

MalVal *mal_error = NULL; // WARNGIN: global state
void _error(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    mal_error = malval_new_string(g_strdup_vprintf(fmt, args));
}

// Constant atomic values

MalVal mal_nil = {MAL_NIL, NULL, {0}, 0};
MalVal mal_true = {MAL_TRUE, NULL, {0}, 0};
MalVal mal_false = {MAL_FALSE, NULL, {0}, 0};


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

MalVal *malval_new_keyword(char *val) {
    MalVal *mv = malval_new(MAL_STRING, NULL);
    mv->val.string = g_strdup_printf("\x7f%s", val);
    return mv;
}

MalVal *malval_new_list(MalType type, GArray *val) {
    MalVal *mv = malval_new(type, NULL);
    mv->val.array = val;
    return mv;
}

MalVal *malval_new_hash_map(GHashTable *val) {
    MalVal *mv = malval_new(MAL_HASH_MAP, NULL);
    mv->val.hash_table = val;
    return mv;
}

MalVal *malval_new_atom(MalVal *val) {
    MalVal *mv = malval_new(MAL_ATOM, NULL);
    mv->val.atom_val = val;
    return mv;
}


MalVal *malval_new_function(void *(*func)(void *), int arg_cnt) {
    MalVal *mv = malval_new(MAL_FUNCTION_C, NULL);
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

MalVal *_apply(MalVal *f, MalVal *args) {
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


// Lists
MalVal *_listX(int count, ...) {
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

MalVal *_list(MalVal *args) {
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "list called with invalid arguments");
    args->type = MAL_LIST;
    return args;
}

int _list_Q(MalVal *seq) {
    return seq->type & MAL_LIST;
}


// Vectors
MalVal *_vector(MalVal *args) {
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "vector called with invalid arguments");
    args->type = MAL_VECTOR;
    return args;
}

int _vector_Q(MalVal *seq) {
    return seq->type & MAL_VECTOR;
}


// Hash maps
MalVal *_hash_map(MalVal *args) {
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "hash-map called with non-sequential arguments");
    GHashTable *htable = g_hash_table_new(g_str_hash, g_str_equal);
    MalVal *hm = malval_new_hash_map(htable);
    return _assoc_BANG(hm, args);
}

int _hash_map_Q(MalVal *seq) {
    return seq->type & MAL_HASH_MAP;
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


// Atoms
int _atom_Q(MalVal *exp) {
    return exp->type & MAL_ATOM;
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

MalVal *_nth(MalVal *seq, int idx) {
    assert_type(seq, MAL_LIST|MAL_VECTOR,
                "_nth called with non-sequential");
    if (idx >= _count(seq)) {
        abort("nth: index out of range");
    }
    return g_array_index(seq->val.array, MalVal*, idx);
}

MalVal *_first(MalVal *seq) {
    assert_type(seq, MAL_LIST|MAL_VECTOR,
                "_first called with non-sequential");
    if (_count(seq) == 0) {
        return &mal_nil;
    }
    return g_array_index(seq->val.array, MalVal*, 0);
}

MalVal *_last(MalVal *seq) {
    assert_type(seq, MAL_LIST|MAL_VECTOR,
                "_last called with non-sequential");
    if (_count(seq) == 0) {
        return &mal_nil;
    }
    return g_array_index(seq->val.array, MalVal*, _count(seq)-1);
}


MalVal *_rest(MalVal *seq) {
    return _slice(seq, 1, _count(seq));
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
