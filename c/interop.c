#include <string.h>
#include <dlfcn.h>
#if OSX
  #include <ffi/ffi.h>
#else
  #include <ffi.h>
#endif

#include "types.h"


GHashTable *loaded_dls = NULL;

int get_byte_size(char *type) {
    return 0;
}

typedef struct Raw64 {
    union {
        gdouble floatnum;
        gint64  integernum;
        char   *string;
    } v;
} Raw64;


// obj must be a pointer to the object to store
ffi_type *_get_ffi_type(char *type) {
    if ((strcmp("void", type) == 0)) {
        return &ffi_type_void;
    } else if ((strcmp("string", type) == 0) ||
               (strcmp("char*", type) == 0) ||
               (strcmp("char *", type) == 0)) {
        return &ffi_type_pointer;
    } else if ((strcmp("integer", type) == 0) ||
               (strcmp("int64", type) == 0)) {
        return &ffi_type_sint64;
    } else if ((strcmp("int32", type) == 0)) {
        return &ffi_type_sint32;
    } else if (strcmp("double", type) == 0) {
        return &ffi_type_double;
    } else if (strcmp("float", type) == 0) {
        return &ffi_type_float;
    } else {
        abort("_get_ffi_type of unknown type '%s'", type);
    }
}

MalVal *_malval_new_by_type(char *type) {
    if ((strcmp("void", type) == 0)) {
        return NULL;
    } else if ((strcmp("string", type) == 0) ||
               (strcmp("char*", type) == 0) ||
               (strcmp("char *", type) == 0)) {
        return malval_new(MAL_STRING, NULL);
    } else if ((strcmp("integer", type) == 0) ||
               (strcmp("int64", type) == 0)) {
        return malval_new(MAL_INTEGER, NULL);
    } else if ((strcmp("int32", type) == 0)) {
        return malval_new(MAL_INTEGER, NULL);
    } else if (strcmp("double", type) == 0) {
        return malval_new(MAL_FLOAT, NULL);
    } else if (strcmp("float", type) == 0) {
        return malval_new(MAL_FLOAT, NULL);
    } else {
        abort("_malval_new_by_type of unknown type '%s'", type);
    }
}



// Mal syntax:
//   (.  {DYN_LIB_FILE|nil}  RETURN_TYPE  FUNC_NAME  [ARG_TYPE ARG]...)
MalVal *invoke_native(MalVal *call_data) {
    //g_print("invoke_native %s\n", pr_str(call_data));
    int cd_len = call_data->val.array->len;
    int arg_len = (cd_len - 3)/2;
    char *error;
    void *dl_handle;

    assert_type(call_data, MAL_LIST,
                "invoke_native called with non-list call_data: %s",
                _pr_str(call_data,1));
    assert(cd_len >= 3,
           "invoke_native called with %d args, needs at least 3",
           cd_len);
    assert((cd_len % 2) == 1,
           "invoke_native called with an even number of args (%d)",
           cd_len);
    assert(arg_len <= 3,
           "invoke_native called with more than 3 native args (%d)",
           arg_len);
    MalVal *dl_file = _nth(call_data, 0),
           *ftype   = _nth(call_data, 1),
           *fname   = _nth(call_data, 2);
    assert_type(dl_file, MAL_STRING|MAL_NIL,
                "invoke_native arg 1 (DYN_LIB_NAME) must be a string or nil");
    assert_type(ftype, MAL_STRING,
                "invoke_native arg 2 (RETURN_TYPE) must be a string");
    assert_type(fname, MAL_STRING,
                "invoke_native arg 3 (FUNC_NAME) must be a string");

    // Cached load of the dynamic library handle
    if (dl_file->type == MAL_NIL) {
        dl_handle = dlopen(NULL, RTLD_LAZY);
    } else {
        // Load the library
        if (loaded_dls == NULL) {
            loaded_dls = g_hash_table_new(g_str_hash, g_str_equal);
        }
        dl_handle = g_hash_table_lookup(loaded_dls, dl_file->val.string);
        dlerror(); // clear any existing error
        if (!dl_handle) {
            dl_handle = dlopen(dl_file->val.string, RTLD_LAZY);
        }
        if ((error = dlerror()) != NULL) {
            abort("Could not dlopen '%s': %s", dl_file->val.string, error);
        }
        g_hash_table_insert(loaded_dls, dl_file->val.string, dl_handle);
    }

    void * func = dlsym(dl_handle, fname->val.string);
    if ((error = dlerror()) != NULL) {
        abort("Could not dlsym '%s': %s", fname->val.string, error);
    }


    // 
    // Use FFI library to make a dynamic call
    //
    
    // Based on:
    // http://eli.thegreenplace.net/2013/03/04/flexible-runtime-interface-to-shared-libraries-with-libffi/
    ffi_cif cif;
    ffi_type *ret_type;
    ffi_type *arg_types[20];
    void     *arg_vals[20];
    ffi_status status;
    MalVal *ret_mv;

    // Set return type
    ret_type = _get_ffi_type(ftype->val.string);
    ret_mv = _malval_new_by_type(ftype->val.string);
    if (mal_error) { return NULL; }

    // Set the argument types and values
    int i;
    for (i=0; i < arg_len; i++) {
        arg_types[i] = _get_ffi_type(_nth(call_data, 3+i*2)->val.string);
        if (arg_types[i] == NULL) {
            return NULL;
        }
        arg_vals[i] = &_nth(call_data, 4+i*2)->val;
    }

    status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, arg_len,
                          ret_type, arg_types);
    if (status != FFI_OK) {
        abort("ffi_prep_cif failed: %d\n", status);
    }

    // Perform the call
    //g_print("Calling %s[%p](%d)\n", fname->val.string, func, arg_len);
    ffi_call(&cif, FFI_FN(func), &ret_mv->val, arg_vals);

    if (ret_type == &ffi_type_void) {
        return &mal_nil;
    } else {
        return ret_mv;
    }
}

