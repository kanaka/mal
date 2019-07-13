GET "libhdr"
GET "malhdr"

GET "printer.b"
GET "reader.b"
GET "readline.b"
GET "types.b"

LET READ(x) = read_str(x)

LET eval_ast(ast, env) = VALOF
  SWITCHON type OF ast INTO
  { CASE t_sym: RESULTIS env(ast)
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
  ast := eval_ast(ast, env)
  { LET fn, args = ast!lst_first, ast!lst_rest
    UNLESS type OF fn = t_fun DO throwf("not a function")
    RESULTIS (fn!fun_code)(fn, args)
  }
}

LET PRINT(x) = pr_str(x)

STATIC { add_fun; sub_fun; mul_fun; div_fun }

LET init_core() BE
{ MANIFEST { wf_wrapped = 2; wf_sz = 3 }
  LET arith(fn, args) = VALOF
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
  add_fun := arith_fun(add)
  sub_fun := arith_fun(sub)
  mul_fun := arith_fun(mul)
  div_fun := arith_fun(div)
}

LET repl_env(key) = VALOF
{ IF key!str_len = 1 SWITCHON (key + str_data)%1 INTO
  { CASE '+':  RESULTIS add_fun
    CASE '-':  RESULTIS sub_fun
    CASE '**': RESULTIS mul_fun
    CASE '/':  RESULTIS div_fun
  }
  throw(str_bcpl2mal("unknown function"))
}

LET rep(x) = PRINT(EVAL(READ(x), repl_env))

LET repl() BE
{ LET prompt = str_bcpl2mal("user> ")
  catch_level, catch_label := level(), uncaught
  IF FALSE THEN
  { uncaught:
    writes("Uncaught exception: ")
    writes(@(pr_str(last_exception)!str_data))
    newline()
  }
  { LET line = readline(prompt)
    IF line = nil THEN BREAK
    writes(@rep(line)!str_data)
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
