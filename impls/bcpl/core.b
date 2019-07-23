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
  LET bare_fun(fn) = alloc_fun(fn, fun_data)
   
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

  // Predicates
  { LET pred(fn, args) = (fn!wf_wrapped)(args!lst_first) -> mtrue, mfalse
    LET pred_fun(fn) = alloc_fun(pred, wf_sz, fn)

    LET listp(val) = type OF val = t_lst
    // The '->' here is to make sure that | and & are in truth-value
    // context.
    LET emptyp(val) = val =
      empty | supertype OF val = t_vec & val!vec_len = 0 -> TRUE, FALSE

    def(env, "list?", pred_fun(listp))
    def(env, "empty?", pred_fun(emptyp))
  }

  // Comparisons
  { LET equalp(fn, args) = VALOF
    { LET a, b = args!lst_first, args!lst_rest!lst_first
      RESULTIS equal(a, b) -> mtrue, mfalse
    }
    LET cmp(fn, args) = VALOF
    { LET a, b = args!lst_first, args!lst_rest!lst_first
      UNLESS type OF a = type OF b = t_int DO
        throwf("bad arguments for arithmetic function: %v", args)
      RESULTIS  (fn!wf_wrapped)(a!int_value, b!int_value) -> mtrue, mfalse
    }
    LET cmp_fun(fn) = alloc_fun(cmp, wf_sz, fn)

    LET lt(a, b) = a < b
    LET le(a, b) = a <= b
    LET gt(a, b) = a > b
    LET ge(a, b) = a >= b
    
    def(env, "=",  bare_fun(equalp))
    def(env, "<",  cmp_fun(lt))
    def(env, "<=", cmp_fun(le))
    def(env, ">",  cmp_fun(gt))
    def(env, ">=", cmp_fun(ge))
  }

  // Miscellaneous list functions
  { LET count(fn, args) = VALOF
    { LET arg = args!lst_first
      SWITCHON supertype OF arg INTO
      { CASE t_nil: RESULTIS alloc_int(0)
        CASE t_lst:
        { LET n = 0
          UNTIL arg = empty DO n, arg := n + 1, arg!lst_rest
	  RESULTIS alloc_int(n)
	}
        CASE t_vec: RESULTIS alloc_int(arg!vec_len)
	DEFAULT: throwf("invalid argument to count: %v", arg)
      }
    }
    def(env, "count", bare_fun(count))
  }

  // Printing functions
  { LET prstr(fn, args) = pr_multi(args, TRUE, TRUE)
    LET str(fn, args) = pr_multi(args, FALSE, FALSE)
    LET prn(fn, args) = VALOF
    { writes(@(pr_multi(args, TRUE, TRUE)!str_data))
      newline()
      RESULTIS nil
    }
    LET println(fn, args) = VALOF
    { writes(@(pr_multi(args, FALSE, TRUE)!str_data))
      newline()
      RESULTIS nil
    }
    def(env, "pr-str", bare_fun(prstr))
    def(env, "str", bare_fun(str))
    def(env, "prn", bare_fun(prn))
    def(env, "println", bare_fun(println))
  }

  // Constructors
  { LET list(fn, args) = args
    def (env, "list", bare_fun(list))
  }
  RESULTIS env
}
