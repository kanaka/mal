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
      ELSE RESULTIS cons(EVAL(ast!lst_first), eval_ast(ast!lst_rest))
    DEFAULT: RESULTIS ast
  }

AND EVAL(ast) = VALOF
  UNLESS type OF ast = t_lst RESULTIS eval_ast(ast, env)
  IF ast = empty RESULTIS ast
  ast := eval_ast(ast, env)
  
  

LET PRINT(x) = pr_str(x)

LET add(a, b)      = alloc_int(a!int_value + b!int_value)
LET subtract(a, b) = alloc_int(a!int_value - b!int_value)
LET multiply(a, b) = alloc_int(a!int_value * b!int_value)
LET divide(a, b)   = alloc_int(a!int_value / b!int_value)

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
  ch := rdch() REPEATUNTIL ch = '*n' // Consume command-line args
  wrch('*n') // Terminate prompt printed by Cintsys
  repl()
  RESULTIS 0
}
