GET "libhdr"
GET "malhdr"

GET "core.b"
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
    IF is_sym(fn, "do") THEN
    { LET tail = ast!lst_rest
      tail := eval_ast(tail, env)
      UNTIL tail!lst_rest = empty DO tail := tail!lst_rest
      RESULTIS tail!lst_first
    }
    IF is_sym(fn, "if") THEN
    { LET cond, tail = EVAL(nth(ast, 1), env), ast!lst_rest!lst_rest
      IF cond = nil | cond = mfalse THEN tail := tail!lst_rest
      RESULTIS EVAL(tail!lst_first, env)
    }
    IF is_sym(fn, "fn**") THEN
    { MANIFEST { fun_binds = fun_data; fun_body; fun_env; fun_sz }
      LET call(fun, args) =
          EVAL(fun!fun_body, env_new(fun!fun_env, fun!fun_binds, args))
      LET result = alloc_fun(call, fun_sz, nth(ast, 1), nth(ast, 2), env)
      RESULTIS result
    }
  }
  ast := eval_ast(ast, env)
  { LET fn, args = ast!lst_first, ast!lst_rest
    UNLESS type OF fn = t_fun DO throwf("not a function")
    RESULTIS (fn!fun_code)(fn, args)
  }
}

LET PRINT(x) = pr_str(x)

LET rep(x, env) = PRINT(EVAL(READ(x), env))

LET repl() BE
{ LET prompt = str_bcpl2mal("user> ")
  LET repl_env = ?
  catch_level, catch_label := level(), uncaught
  repl_env := core_env()
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
  ch := rdch() REPEATUNTIL ch = '*n' // Consume command-line args
  wrch('*n') // Terminate prompt printed by Cintsys
  repl()
  RESULTIS 0
}