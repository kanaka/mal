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

STATIC { add; subtract; multiply; divide }

LET init_core() BE
{ add      := alloc_fun(arith, add_fn)
  subtract := alloc_fun(arith, subtract_fn)
  multiply := alloc_fun(arith, multiply_fn)
  divide   := alloc_fun(arith, divide_fn)
}

LET repl_env(key) = VALOF
{ IF key!str_len = 1 SWITCHON (key + str_data)%1 INTO
  { CASE '+': RESULTIS add
    CASE '-': RESULTIS subtract
    CASE '**': RESULTIS multiply
    CASE '/': RESULTIS divide
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
