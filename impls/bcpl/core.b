GET "libhdr"
GET "malhdr"

// The Guide would have the file simply declare a data structure
// specifying the mal core namespace, with the step files constructing
// repl_env based on it.  This is not really appropriate in BCPL,
// though.  BCPL has almost no facilities for defining static data.
// There is TABLE, but that's restricted to compile-time constants.
// Pointers in BCPL are not compile-time constants, so a TABLE cannot
// contain any strings, or pointers to routines, or pointers to other
// TABLEs.  This means that core.ns would have to be defined by
// a function constructing it, but if we are going to do that, we may
// as well have the function construct an entire environment instead.

LET core_env() = VALOF
{ LET env = env_new(nil, empty, empty)
  LET def(env, name, value) BE env_set(env, as_sym(str_bcpl2mal(name)), value)

  // A common form of core function is a wrapped function, where one
  // function does the actual work while another (the wrapper) handles
  // conversion between mal and BCPL conventions.  The wrapper
  // functions tend to be more complicated, but they can be shared
  // between many wrapped functions.
  MANIFEST { wf_wrapped = fun_data; wf_sz }

  // Arithmetic functions
  { LET arith(fn, args) = VALOF
    { LET a, b = args!lst_first, args!lst_rest!lst_first
      UNLESS type OF a = type OF b = t_int DO
        throwf("bad arguments for arithmetic function: %v", args)
      RESULTIS alloc_int((fn!wf_wrapped)(a!int_value, b!int_value))
    }
    LET arith_fun(fn) = alloc_fun(arith, wf_sz, fn)

    LET add(a, b) = a + b
    LET sub(a, b) = a - b
    LET mul(a, b) = a * b
    LET div(a, b) = VALOF
    { IF b = 0 THEN throwf("division by zero")
      RESULTIS a / b
    }

    def(env, "+",  arith_fun(add))
    def(env, "-",  arith_fun(sub))
    def(env, "**", arith_fun(mul))
    def(env, "/",  arith_fun(div))
  }

  // Printing functions
  { LET bare_fun(fn) = alloc_fun(fn, fun_data)
    LET prn(fn, args) = VALOF
    { writes(@(pr_str(args!lst_first)!str_data))
      newline()
      RESULTIS nil
    }
    def(env, "prn", bare_fun(prn))
  }

  RESULTIS env
}
