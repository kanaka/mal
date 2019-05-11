#ifndef __MAL_CORE__
#define __MAL_CORE__

#include <stddef.h>

#include "env.h"
#include "types.h"

struct hashmap* core_ns();
MalType* core_add(MalEnv *env, size_t argc, MalType **args);
MalType* core_sub(MalEnv *env, size_t argc, MalType **args);
MalType* core_mul(MalEnv *env, size_t argc, MalType **args);
MalType* core_div(MalEnv *env, size_t argc, MalType **args);
MalType* core_count(MalEnv *env, size_t argc, MalType **args);
MalType* core_prn(MalEnv *env, size_t argc, MalType **args);
MalType* core_list(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_list(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_empty(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_equal(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_gt(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_gte(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_lt(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_lte(MalEnv *env, size_t argc, MalType **args);
MalType* core_pr_str(MalEnv *env, size_t argc, MalType **args);
MalType* core_str(MalEnv *env, size_t argc, MalType **args);
MalType* core_println(MalEnv *env, size_t argc, MalType **args);
MalType* core_read_string(MalEnv *env, size_t argc, MalType **args);
MalType* core_slurp(MalEnv *env, size_t argc, MalType **args);
MalType* core_atom(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_atom(MalEnv *env, size_t argc, MalType **args);
MalType* core_deref(MalEnv *env, size_t argc, MalType **args);
MalType* core_reset(MalEnv *env, size_t argc, MalType **args);
MalType* core_swap(MalEnv *env, size_t argc, MalType **args);
MalType* core_cons(MalEnv *env, size_t argc, MalType **args);
MalType* core_concat(MalEnv *env, size_t argc, MalType **args);
MalType* core_nth(MalEnv *env, size_t argc, MalType **args);
MalType* core_first(MalEnv *env, size_t argc, MalType **args);
MalType* core_rest(MalEnv *env, size_t argc, MalType **args);
MalType* core_throw(MalEnv *env, size_t argc, MalType **args);
MalType* core_apply(MalEnv *env, size_t argc, MalType **args);
MalType* core_map(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_nil(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_true(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_false(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_symbol(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_keyword(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_vector(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_map(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_sequential(MalEnv *env, size_t argc, MalType **args);
MalType* core_symbol(MalEnv *env, size_t argc, MalType **args);
MalType* core_keyword(MalEnv *env, size_t argc, MalType **args);
MalType* core_vector(MalEnv *env, size_t argc, MalType **args);
MalType* core_hash_map(MalEnv *env, size_t argc, MalType **args);
MalType* core_assoc(MalEnv *env, size_t argc, MalType **args);
MalType* core_dissoc(MalEnv *env, size_t argc, MalType **args);
MalType* core_get(MalEnv *env, size_t argc, MalType **args);
MalType* core_contains(MalEnv *env, size_t argc, MalType **args);
MalType* core_keys(MalEnv *env, size_t argc, MalType **args);
MalType* core_vals(MalEnv *env, size_t argc, MalType **args);
MalType* core_readline(MalEnv *env, size_t argc, MalType **args);
MalType* core_meta(MalEnv *env, size_t argc, MalType **args);
MalType* core_with_meta(MalEnv *env, size_t argc, MalType **args);
MalType* core_seq(MalEnv *env, size_t argc, MalType **args);
MalType* core_conj(MalEnv *env, size_t argc, MalType **args);
MalType* core_time_ms(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_string(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_number(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_fn(MalEnv *env, size_t argc, MalType **args);
MalType* core_is_macro(MalEnv *env, size_t argc, MalType **args);
void add_core_ns_to_env(MalEnv *env);

#endif
