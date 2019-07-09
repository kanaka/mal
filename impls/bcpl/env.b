GET "libhdr"
GET "malhdr"

MANIFEST
{ env_outer = 0; env_data; env_sz }

LET env_new(outer) = VALOF
{ LET env = getvec(env_sz)
  env!env_outer := outer
  env!env_data := empty_hashmap
  RESULTIS env
}

LET env_set(env, key, value) BE
  env!env_data := hm_set(env!env_data, key, value)

LET env_find(env, key) = VALOF
{ IF hm_contains(env!env_data, key) THEN RESULTIS env
  UNLESS env!env_outer = nil RESULTIS env_find(env!env_outer, key)
  RESULTIS nil
}

LET env_get(env, key) = VALOF
{ env := env_find(env, key)
  IF env = nil THEN throwf("symbol %v not found", key)
  RESULTIS hm_get(env!env_data, key)
}
