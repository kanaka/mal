GET "libhdr"
GET "malhdr"

GET "printer.b"
GET "reader.b"
GET "readline.b"
GET "types.b"

LET READ(x) = read_str(x)

LET EVAL(x) = x

LET PRINT(x) = pr_str(x)

LET rep(x) = PRINT(EVAL(READ(x)))

LET repl() BE
{ catch_level, catch_label := level(), uncaught
  IF FALSE THEN
  { uncaught:
    writes("Uncaught exception: ")
    writes(@(pr_str(last_exception)!str_data))
    newline()
  }
  { LET prompt = str_bcpl2mal("user> ")
    LET line = readline(prompt)
    IF line = nil THEN BREAK
    writes(@rep(line)!str_data)
    newline()
    gc(nil)
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
