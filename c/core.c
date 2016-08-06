#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <fcntl.h>
#include <unistd.h>

#include "types.h"
#include "core.h"
#include "reader.h"
#include "printer.h"

// Errors/Exceptions
void throw(MalVal *obj) {
    mal_error = obj;
}


// General functions

MalVal *equal_Q(MalVal *a, MalVal *b) {
    if (_equal_Q(a, b)) { return &mal_true; }
    else { return &mal_false; }
}


// Scalar functions

MalVal *nil_Q(MalVal *seq) { return seq->type & MAL_NIL ? &mal_true : &mal_false; }
MalVal *true_Q(MalVal *seq) { return seq->type & MAL_TRUE ? &mal_true : &mal_false; }
MalVal *false_Q(MalVal *seq) { return seq->type & MAL_FALSE ? &mal_true : &mal_false; }
MalVal *string_Q(MalVal *seq) {
    if ((seq->type & MAL_STRING) && (seq->val.string[0] != '\x7f')) {
        return &mal_true;
    } else {
        return &mal_false;
    }
}


// Symbol functions

MalVal *symbol(MalVal *args) {
    assert_type(args, MAL_STRING,
                "symbol called with non-string value");
    args->type = MAL_SYMBOL; // change string to symbol
    return args;
}

MalVal *symbol_Q(MalVal *seq) {
    return seq->type & MAL_SYMBOL ? &mal_true : &mal_false; }


// Keyword functions

MalVal *keyword(MalVal *args) {
    assert_type(args, MAL_STRING,
                "keyword called with non-string value");
    if (args->val.string[0] == '\x7f') {
        return args;
    } else {
        return malval_new_keyword(args->val.string);
    }
}

MalVal *keyword_Q(MalVal *seq) {
    return seq->type & MAL_STRING && seq->val.string[0] == '\x7f'
        ? &mal_true
        : &mal_false;
}


// String functions

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
    puts(repr);
    MAL_GC_FREE(repr);
    return &mal_nil;
}

// Print a string representation of a MalVal sequence (for human consumption)
// followed by a newline. Returns nil.
MalVal *println(MalVal *args) {
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "println called with non-sequential args");
    char *repr = _pr_str_args(args, " ", 0);
    puts(repr);
    MAL_GC_FREE(repr);
    return &mal_nil;
}

MalVal *mal_readline(MalVal *str) {
    assert_type(str, MAL_STRING, "readline of non-string");
    char * line = _readline(str->val.string);
    if (line) { return malval_new_string(line); }
    else      { return &mal_nil; }
}

MalVal *read_string(MalVal *str) {
    assert_type(str, MAL_STRING, "read_string of non-string");
    return read_str(str->val.string);
}

char *slurp_raw(char *path) {
    char *data;
    struct stat fst;
    int fd = open(path, O_RDONLY),
        sz;
    if (fd < 0) {
        abort("slurp failed to open '%s'", path);
    }
    if (fstat(fd, &fst) < 0) {
        abort("slurp failed to stat '%s'", path);
    }
    data = MAL_GC_MALLOC(fst.st_size+1);
    sz = read(fd, data, fst.st_size);
    if (sz < fst.st_size) {
        abort("slurp failed to read '%s'", path);
    }
    data[sz] = '\0';
    return data;
}
MalVal *slurp(MalVal *path) {
    assert_type(path, MAL_STRING, "slurp of non-string");
    char *data = slurp_raw(path->val.string);
    if (!data || mal_error) { return NULL; }
    return malval_new_string(data);
}




// Number functions

WRAP_INTEGER_OP(plus,+)
WRAP_INTEGER_OP(minus,-)
WRAP_INTEGER_OP(multiply,*)
WRAP_INTEGER_OP(divide,/)
WRAP_INTEGER_CMP_OP(gt,>)
WRAP_INTEGER_CMP_OP(gte,>=)
WRAP_INTEGER_CMP_OP(lt,<)
WRAP_INTEGER_CMP_OP(lte,<=)

MalVal *time_ms(MalVal *_) {
    struct timeval tv;
    long msecs;
    gettimeofday(&tv, NULL);
    msecs = tv.tv_sec * 1000 + tv.tv_usec/1000.0 + 0.5;

    return malval_new_integer(msecs);
}


// List functions

MalVal *list(MalVal *args) { return _list(args); }
MalVal *list_Q(MalVal *seq) { return _list_Q(seq) ? &mal_true : &mal_false; }


// Vector functions

MalVal *vector(MalVal *args) { return _vector(args); }
MalVal *vector_Q(MalVal *seq) { return _vector_Q(seq) ? &mal_true : &mal_false; }


// Hash map functions

MalVal *hash_map_Q(MalVal *seq) { return _hash_map_Q(seq) ? &mal_true : &mal_false; }

MalVal *assoc(MalVal *args) {
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "assoc called with non-sequential arguments");
    assert(_count(args) >= 2,
           "assoc needs at least 2 arguments");
    GHashTable *htable = g_hash_table_copy(_first(args)->val.hash_table);
    MalVal *hm = malval_new_hash_map(htable);
    return _assoc_BANG(hm, _rest(args));
}

MalVal *dissoc(MalVal* args) {
    GHashTable *htable = g_hash_table_copy(_first(args)->val.hash_table);
    MalVal *hm = malval_new_hash_map(htable);
    return _dissoc_BANG(hm, _rest(args));
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
    case MAL_NIL:
        return &mal_nil;
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


// Sequence functions

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

MalVal *nth(MalVal *seq, MalVal *idx) {
    return _nth(seq, idx->val.intnum);
}

MalVal *empty_Q(MalVal *seq) {
    assert_type(seq, MAL_LIST|MAL_VECTOR,
                "empty? called with non-sequential");
    return (seq->val.array->len == 0) ? &mal_true : &mal_false;
}

MalVal *count(MalVal *seq) {
    return malval_new_integer(_count(seq));
}

MalVal *apply(MalVal *args) {
    assert_type(args, MAL_LIST|MAL_VECTOR,
                "apply called with non-sequential");
    MalVal *f = _nth(args, 0);
    MalVal *last_arg = _last(args);
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
    return _apply(f, malval_new_list(MAL_LIST, new_arr));
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

MalVal *seq(MalVal *obj) {
    assert_type(obj, MAL_LIST|MAL_VECTOR|MAL_STRING|MAL_NIL,
                "seq: called with non-sequential");
    int cnt, i;
    MalVal *lst, *mstr;
    switch (obj->type) {
    case MAL_LIST:
        cnt = _count(obj);
        if (cnt == 0) { return &mal_nil; }
        return obj;
    case MAL_VECTOR:
        cnt = _count(obj);
        if (cnt == 0) { return &mal_nil; }
        lst = malval_new_list(MAL_LIST,
                            g_array_sized_new(TRUE, TRUE, sizeof(MalVal*), cnt));
        lst->val.array = obj->val.array;
        return lst;
    case MAL_STRING:
        cnt = strlen(obj->val.string);
        if (cnt == 0) { return &mal_nil; }
        lst = malval_new_list(MAL_LIST,
                              g_array_sized_new(TRUE, TRUE, sizeof(MalVal*), cnt));
        for (i=0; i<cnt; i++) {
            mstr = malval_new_string(g_strdup_printf("%c", obj->val.string[i]));
            g_array_append_val(lst->val.array, mstr);
        }
        return lst;
    case MAL_NIL:
        return &mal_nil;
    }
}


// Metadata functions

MalVal *with_meta(MalVal *obj, MalVal *meta) {
    MalVal *new_obj = malval_new(obj->type, meta);
    new_obj->val = obj->val;
    return new_obj;
}

MalVal *meta(MalVal *obj) {
    assert_type(obj, MAL_LIST|MAL_VECTOR|MAL_HASH_MAP|
                     MAL_FUNCTION_C|MAL_FUNCTION_MAL|MAL_ATOM,
                "attempt to get metadata from non-collection type");
    if (obj->metadata == NULL) {
        return &mal_nil;
    } else {
        return obj->metadata;
    }
}


// Atoms

MalVal *atom(MalVal *val) {
    return malval_new_atom(val);
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
           *new_val = _apply(f, fargs);
    if (mal_error) { return NULL; }
    atm->val.atom_val = new_val;
    return new_val;
}



core_ns_entry core_ns[58] = {
    {"=", (void*(*)(void*))equal_Q, 2},
    {"throw", (void*(*)(void*))throw, 1},
    {"nil?", (void*(*)(void*))nil_Q, 1},
    {"true?", (void*(*)(void*))true_Q, 1},
    {"false?", (void*(*)(void*))false_Q, 1},
    {"string?", (void*(*)(void*))string_Q, 1},
    {"symbol", (void*(*)(void*))symbol, 1},
    {"symbol?", (void*(*)(void*))symbol_Q, 1},
    {"keyword", (void*(*)(void*))keyword, 1},
    {"keyword?", (void*(*)(void*))keyword_Q, 1},

    {"pr-str", (void*(*)(void*))pr_str, -1},
    {"str", (void*(*)(void*))str, -1},
    {"prn", (void*(*)(void*))prn, -1},
    {"println", (void*(*)(void*))println, -1},
    {"readline", (void*(*)(void*))mal_readline, 1},
    {"read-string", (void*(*)(void*))read_string, 1},
    {"slurp", (void*(*)(void*))slurp, 1},
    {"<", (void*(*)(void*))int_lt, 2},
    {"<=", (void*(*)(void*))int_lte, 2},
    {">", (void*(*)(void*))int_gt, 2},
    {">=", (void*(*)(void*))int_gte, 2},
    {"+", (void*(*)(void*))int_plus, 2},
    {"-", (void*(*)(void*))int_minus, 2},
    {"*", (void*(*)(void*))int_multiply, 2},
    {"/", (void*(*)(void*))int_divide, 2},
    {"time-ms", (void*(*)(void*))time_ms, 0},

    {"list", (void*(*)(void*))list, -1},
    {"list?", (void*(*)(void*))list_Q, 1},
    {"vector", (void*(*)(void*))vector, -1},
    {"vector?", (void*(*)(void*))vector_Q, 1},
    {"hash-map", (void*(*)(void*))_hash_map, -1},
    {"map?", (void*(*)(void*))hash_map_Q, 1},
    {"assoc", (void*(*)(void*))assoc, -1},
    {"dissoc", (void*(*)(void*))dissoc, -1},
    {"get", (void*(*)(void*))get, 2},
    {"contains?", (void*(*)(void*))contains_Q, 2},
    {"keys", (void*(*)(void*))keys, 1},
    {"vals", (void*(*)(void*))vals, 1},

    {"sequential?", (void*(*)(void*))sequential_Q, 1},
    {"cons", (void*(*)(void*))cons, 2},
    {"concat", (void*(*)(void*))concat, -1},
    {"nth", (void*(*)(void*))nth, 2},
    {"first", (void*(*)(void*))_first, 1},
    {"rest", (void*(*)(void*))_rest, 1},
    {"last", (void*(*)(void*))_last, 1},
    {"empty?", (void*(*)(void*))empty_Q, 1},
    {"count", (void*(*)(void*))count, 1},
    {"apply", (void*(*)(void*))apply, -1},
    {"map", (void*(*)(void*))map, 2},

    {"conj", (void*(*)(void*))sconj, -1},
    {"seq", (void*(*)(void*))seq, 1},

    {"with-meta", (void*(*)(void*))with_meta, 2},
    {"meta", (void*(*)(void*))meta, 1},
    {"atom", (void*(*)(void*))atom, 1},
    {"atom?", (void*(*)(void*))atom_Q, 1},
    {"deref", (void*(*)(void*))deref, 1},
    {"reset!", (void*(*)(void*))reset_BANG, 2},
    {"swap!", (void*(*)(void*))swap_BANG, -1},
    };
