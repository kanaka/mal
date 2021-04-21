GET "libhdr"
GET "malhdr"

LET env_set(env, key, value) BE
  env!env_data := hm_set(env!env_data, key, value)

LET env_new(outer, binds, exprs) = VALOF
{ LET env = alloc_val(env_sz)
  type OF env := t_env
  env!env_outer := outer
  env!env_data := empty_hashmap
  UNTIL binds = empty DO
  { IF str_eq_const(binds!lst_first, "&") THEN
    { env_set(env, nth(binds, 1), exprs)
      BREAK
    }
    env_set(env, binds!lst_first, exprs!lst_first)
    binds, exprs := binds!lst_rest, exprs!lst_rest
  }
  RESULTIS env
}

LET env_find(env, key) = VALOF
{ IF hm_contains(env!env_data, key) THEN RESULTIS env
  UNLESS env!env_outer = nil RESULTIS env_find(env!env_outer, key)
  RESULTIS nil
}

LET env_get(env, key) = VALOF
{ env := env_find(env, key)
  IF env = nil THEN throwf("'%v' not found", key)
  RESULTIS hm_get(env!env_data, key)
}
