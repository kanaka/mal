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
    LET atomp(val) = type OF val = t_atm
    LET nilp(val) = val = nil
    LET truep(val) = val = mtrue
    LET falsep(val) = val = mfalse
    LET symbolp(val) = type OF val = t_sym
    LET keywordp(val) = type OF val = t_kwd
    LET vectorp(val) = type OF val = t_vec
    LET sequentialp(val) = type OF val = t_lst | type OF val = t_vec
    LET mapp(val) = type OF val = t_hm0 | supertype OF val = t_hmi

    def(env, "list?", pred_fun(listp))
    def(env, "empty?", pred_fun(emptyp))
    def(env, "atom?", pred_fun(atomp))
    def(env, "nil?", pred_fun(nilp))
    def(env, "true?", pred_fun(truep))
    def(env, "false?", pred_fun(falsep))
    def(env, "symbol?", pred_fun(symbolp))
    def(env, "keyword?", pred_fun(keywordp))
    def(env, "vector?", pred_fun(vectorp))
    def(env, "sequential?", pred_fun(sequentialp))
    def(env, "map?", pred_fun(mapp))
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
  { LET core_cons(fn, args) =
      cons(args!lst_first, as_lst(args!lst_rest!lst_first))
    LET concat(fn, args) = VALOF
    { LET head, tailp = empty, @head
      IF args = empty RESULTIS empty
      UNTIL args!lst_rest = empty DO
      { LET this = as_lst(args!lst_first)
        UNTIL this = empty DO
	{ !tailp := cons(this!lst_first, empty)
	  tailp := @(!tailp)!lst_rest
	  this := this!lst_rest
	}
	args := args!lst_rest
      }
      !tailp := as_lst(args!lst_first)
      RESULTIS head
    }
    LET count(fn, args) = VALOF
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
    LET core_nth(fn, args) = VALOF
    { LET seq, n = args!lst_first, args!lst_rest!lst_first
      UNLESS type OF n = t_int DO throwf("subscript not an integer")
      n := n!int_value
      IF type OF seq = t_lst RESULTIS nth(seq, n)
      IF type OF seq = t_vec THEN
      { IF n >= seq!vec_len THEN throwf("subscript out of range")
        RESULTIS (seq+vec_data)!n
      }
      throwf("nth applied to non-sequence")
    }
    LET first(fn, args) = VALOF
    { LET seq = args!lst_first
      IF type OF seq = t_vec & seq!vec_len > 0 RESULTIS seq!vec_data
      RESULTIS as_lst(seq)!lst_first
    }
    LET rest(fn, args) = as_lst(args!lst_first)!lst_rest
    def(env, "cons", bare_fun(core_cons))
    def(env, "concat", bare_fun(concat))
    def(env, "count", bare_fun(count))
    def(env, "nth", bare_fun(core_nth))
    def(env, "first", bare_fun(first))
    def(env, "rest", bare_fun(rest))
  }

  // Reading function
  { LET read_string(fn, args) = VALOF
    { UNLESS type OF (args!lst_first) = t_str DO
        throwf("invalid argument to read-string: %v", args!lst_first)
      RESULTIS read_str(args!lst_first)
    }
    def(env, "read-string", bare_fun(read_string))
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

  // File-access function
  { LET slurp(fn, args) = VALOF
    { LET scb = ?
      LET oldcis = cis
      LET dest, dest_size, ptr = ?, 1024, 1
      UNLESS type OF (args!lst_first) = t_str DO
        throwf("invalid argument to slurp: %v", args!lst_first)
      scb := findinput(@(args!lst_first!str_data))
      IF scb = 0 THEN
        throwf("couldn't open %v for input", args!lst_first)
      // rdch() only reads from the current input stream, cis.
      cis := scb
      dest := alloc_str(dest_size)
      { LET c = rdch()
        IF c = endstreamch BREAK
	IF ptr >= dest_size THEN
	{ LET tmp = ?
	  dest_size := dest_size * 2
	  tmp := alloc_str(dest_size)
	  FOR i = 1 TO str_data + dest_size / bytesperword DO
            tmp!i := dest!i
          dest := tmp
	}
	(dest + str_data)%ptr := c
	ptr := ptr + 1
      } REPEAT
      str_setlen(dest, ptr - 1)
      cis := oldcis
      endstream(scb)
      RESULTIS dest
    }
    def(env, "slurp", bare_fun(slurp))
  }

  // Constructors
  { LET list(fn, args) = args
    LET str_conv(fn, args) = VALOF
    { UNLESS supertype OF (args!lst_first) = t_str DO
        throwf("Cannot treat %v as a string", args!lst_first)
      RESULTIS (fn!wf_wrapped)(args!lst_first)
    }
    LET vector(fn, args) = VALOF
    { LET vec = ?
      LET ptr, n = args, 0
      UNTIL ptr = empty DO
        n, ptr := n + 1, ptr!lst_rest
      vec := alloc_vec(n)
      FOR i = 0 TO n - 1 DO
        (vec+vec_data)!i, args := args!lst_first, args!lst_rest
      RESULTIS vec
    }
    def(env, "list", bare_fun(list))
    def(env, "symbol", alloc_fun(str_conv, wf_sz, as_sym))
    def(env, "keyword", alloc_fun(str_conv, wf_sz, as_kwd))
    def(env, "vector", bare_fun(vector))
  }

  // Hash-map functions
  { LET assert_hashmap(hm) = VALOF
    { UNLESS hm = empty_hashmap | supertype OF hm = t_hmi DO
        throwf("Not a hash-map: %v", hm)
      RESULTIS hm
    }
    LET assoc(fn, args) = VALOF
    { LET hm = assert_hashmap(args!lst_first)
      args := args!lst_rest
      UNTIL args = empty DO
      { hm := hm_set(hm, args!lst_first, args!lst_rest!lst_first)
        args := args!lst_rest!lst_rest
      }
      RESULTIS hm
    }
    LET hash_map(fn, args) = assoc(fn, cons(empty_hashmap, args))
    LET dissoc(fn, args) = VALOF
    { LET hm = assert_hashmap(args!lst_first)
      args := args!lst_rest
      UNTIL args = empty DO
        hm, args := hm_remove(hm, args!lst_first), args!lst_rest
      RESULTIS hm
    }
    LET get(fn, args) =
      hm_get(assert_hashmap(args!lst_first), args!lst_rest!lst_first)
    LET containsp(fn, args) =
      hm_contains(assert_hashmap(args!lst_first), args!lst_rest!lst_first) ->
        mtrue, mfalse
    LET fields(fn, args, field) = VALOF
    { LET hm = assert_hashmap(args!lst_first)
      LET fields1(hm, field, acc) = VALOF
      { IF type OF hm = t_hmx RESULTIS cons(hm!field, acc)
        acc := fields1(hm!hmi_right, field, acc)
        RESULTIS fields1(hm!hmi_left, field, acc)
      }
      IF type OF hm = t_hm0 RESULTIS empty
      RESULTIS fields1(hm, field, empty)
    }
    LET keys(fn, args) = fields(fn, args, hmx_key)
    LET vals(fn, args) = fields(fn, args, hmx_value)
    def(env, "hash-map", bare_fun(hash_map))
    def(env, "assoc", bare_fun(assoc))
    def(env, "dissoc", bare_fun(dissoc))
    def(env, "get", bare_fun(get))
    def(env, "contains?", bare_fun(containsp))
    def(env, "keys", bare_fun(keys))
    def(env, "vals", bare_fun(vals))
  }

  // Atom functions
  { LET atom(fn, args) = alloc_atm(args!lst_first)
    LET deref(fn, args) = VALOF
    { UNLESS type OF (args!lst_first) = t_atm DO
        throwf("invalid argument to deref: %v", args!lst_first)
      RESULTIS args!lst_first!atm_value
    }
    LET reset(fn, args) = VALOF
    { UNLESS type OF (args!lst_first) = t_atm DO
        throwf("invalid argument to reset!: %v", args!lst_first)
      args!lst_first!atm_value := args!lst_rest!lst_first
      RESULTIS args!lst_rest!lst_first
    }
    LET swap(fn, args, gc_root) = VALOF
    { LET atm, fn = args!lst_first, args!lst_rest!lst_first
      LET gc_inner_root = cons(atm, gc_root)
      UNLESS type OF atm = t_atm & supertype OF fn = t_fun DO
        throwf("invalid arguments to swap!: %v", args)
      atm!atm_value :=
        (fn!fun_code)(fn, cons(atm!atm_value, args!lst_rest!lst_rest),
		      gc_inner_root)
      RESULTIS atm!atm_value
    }

    def(env, "atom", bare_fun(atom))
    def(env, "deref", bare_fun(deref))
    def(env, "reset!", bare_fun(reset))
    def(env, "swap!", bare_fun(swap))
  }

  // Control-flow functions
  { LET core_throw(fn, args) = throw(args!lst_first)
    LET apply(fn, args, gc_root) = VALOF {
      LET inner_fn, head, tailp = args!lst_first, empty, @head
      args := args!lst_rest
      UNTIL args!lst_rest = empty DO
      { !tailp := cons(args!lst_first, empty)
        tailp := @(!tailp)!lst_rest
	args := args!lst_rest
      }
      !tailp := as_lst(args!lst_first)
      RESULTIS (inner_fn!fun_code)(inner_fn, head, gc_root)
    }
    LET map(fn, args, gc_root) = VALOF
    { LET inner_fn, seq = args!lst_first, as_lst(args!lst_rest!lst_first)
      LET dummy_head, tail = cons(nil, empty), dummy_head
      LET gc_inner_root = alloc_vecn(4, seq, dummy_head, inner_fn, gc_root)
      UNTIL seq = empty DO
      { tail!lst_rest :=
          cons((inner_fn!fun_code)(inner_fn, cons(seq!lst_first, nil),
                                   gc_inner_root), empty)
        tail, seq := tail!lst_rest, seq!lst_rest
      }
      RESULTIS dummy_head!lst_rest
    }
    def(env, "throw", bare_fun(core_throw))
    def(env, "apply", bare_fun(apply))
    def(env, "map", bare_fun(map))
  }
  RESULTIS env
}
