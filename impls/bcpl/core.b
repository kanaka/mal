GET "libhdr"
GET "malhdr"

LET core_env() = VALOF
{ LET env = env_new(nil, empty, empty)
  LET env_set_const(env, name, value) BE
    env_set(env, as_sym(str_bcpl2mal(name)), value)

  { MANIFEST { fun_wrapped = fun_data; fun_sz }
    LET arith(fn, args) = VALOF
    { LET a, b = args!lst_first, args!lst_rest!lst_first
      UNLESS type OF a = type OF b = t_int DO
        throwf("bad arguments for arithmetic function")
      RESULTIS alloc_int((fn!fun_wrapped)(a!int_value, b!int_value))
    }
    LET add_fn(a, b)      = a + b
    LET subtract_fn(a, b) = a - b
    LET multiply_fn(a, b) = a * b
    LET divide_fn(a, b)   = VALOF
    { IF b = 0 THEN throwf("division by zero")
      RESULTIS a / b
    }

    env_set_const(env, "+", alloc_fun(arith, fun_sz, add_fn))
    env_set_const(env, "-", alloc_fun(arith, fun_sz, subtract_fn))
    env_set_const(env, "**", alloc_fun(arith, fun_sz, multiply_fn))
    env_set_const(env, "/", alloc_fun(arith, fun_sz, divide_fn))
  }

  RESULTIS env
}
