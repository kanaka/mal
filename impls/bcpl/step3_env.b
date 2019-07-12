GET "libhdr"
GET "malhdr"

GET "env.b"
GET "printer.b"
GET "reader.b"
GET "readline.b"
GET "types.b"

LET READ(x) = read_str(x)

// Helper function for EVAL.
LET is_sym(a, b) = VALOF
{ UNLESS type OF a = t_sym RESULTIS FALSE
  RESULTIS str_eq_const(a, b)
}

LET eval_ast(ast, env) = VALOF
  SWITCHON type OF ast INTO
  { CASE t_sym: RESULTIS env_get(env, ast)
    CASE t_lst:
      TEST ast = empty THEN RESULTIS empty
      ELSE RESULTIS cons(EVAL(ast!lst_first, env), eval_ast(ast!lst_rest, env))
    CASE t_vec:
      { LET new = alloc_vec(ast!vec_len)
        FOR i = 0 TO ast!vec_len - 1 DO
	  (new + vec_data)!i := EVAL((ast + vec_data)!i, env)
	RESULTIS new
      }
    CASE t_hmx:
      RESULTIS alloc_hmx(ast!hmx_key, EVAL(ast!hmx_value, env))
    CASE t_hmi:
      RESULTIS alloc_hmi(hmi_critbit OF ast, eval_ast(ast!hmi_left, env),
                                             eval_ast(ast!hmi_right, env))
    DEFAULT: RESULTIS ast
  }

AND EVAL(ast, env) = VALOF
{ UNLESS type OF ast = t_lst RESULTIS eval_ast(ast, env)
  IF ast = empty RESULTIS ast
  { LET fn = ast!lst_first
    IF is_sym(fn, "def!") THEN
    { LET val = EVAL(nth(ast, 2), env)
      env_set(env, nth(ast, 1), val, env)
      RESULTIS val
    }
    IF is_sym(fn, "let**") THEN
    { LET newenv, bindings = env_new(env, empty, empty), as_lst(nth(ast, 1))
      UNTIL bindings = empty DO
      { env_set(newenv, bindings!lst_first, EVAL(nth(bindings, 1), newenv))
        bindings := bindings!lst_rest!lst_rest
      }
      RESULTIS EVAL(nth(ast, 2), newenv)
    }
  }
  ast := eval_ast(ast, env)
  { LET fn, args = ast!lst_first, ast!lst_rest
    UNLESS type OF fn = t_fun DO throwf("not a function")
    RESULTIS (fn!fun_code)(fn, args)
  }
}

LET PRINT(x) = pr_str(x)

STATIC { add; subtract; multiply; divide }

LET init_core() BE
{ MANIFEST { fun_wrapped = 2; fun_sz = 3 }
  LET arith(fn, args) = VALOF
  { LET a, b = args!lst_first, args!lst_rest!lst_first
    UNLESS type OF a = type OF b = t_int DO
      throwf("bad arguments for arithmetic function")
    RESULTIS alloc_int((fn!fun_wrapped)(a!int_value, b!int_value))
  }
  LET add_fn(a, b)      = a + b
  LET subtract_fn(a, b) = a - b
  LET multiply_fn(a, b) = a * b
  LET divide_fn(a, b)   = a / b
  add      := alloc_fun(arith, fun_sz, add_fn)
  subtract := alloc_fun(arith, fun_sz, subtract_fn)
  multiply := alloc_fun(arith, fun_sz, multiply_fn)
  divide   := alloc_fun(arith, fun_sz, divide_fn)
}

LET rep(x, env) = PRINT(EVAL(READ(x), env))

LET repl() BE
{ LET prompt = str_bcpl2mal("user> ")
  LET repl_env = env_new(nil, empty, empty)
  env_set(repl_env, as_sym(str_bcpl2mal("+")), add)
  env_set(repl_env, as_sym(str_bcpl2mal("-")), subtract)
  env_set(repl_env, as_sym(str_bcpl2mal("**")), multiply)
  env_set(repl_env, as_sym(str_bcpl2mal("/")), divide)
  catch_level, catch_label := level(), uncaught
  IF FALSE THEN
  { uncaught:
    writes("Uncaught exception: ")
    writes(@(pr_str(last_exception)!str_data))
    newline()
  }
  { LET line = readline(prompt)
    IF line = nil THEN BREAK
    writes(@rep(line, repl_env)!str_data)
    newline()
  } REPEAT
}

LET start() = VALOF
{ LET ch = 0
  init_types()
  init_core()
  ch := rdch() REPEATUNTIL ch = '*n' // Consume command-line args
  wrch('*n') // Terminate prompt printed by Cintsys
  repl()
  RESULTIS 0
}
