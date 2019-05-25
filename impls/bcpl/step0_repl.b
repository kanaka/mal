GET "libhdr"
GET "malhdr"

GET "readline.b"

LET READ(x) = x

LET EVAL(x) = x

LET PRINT(x) = x

LET rep(x) = PRINT(EVAL(READ(x)))

LET repl() BE
{ LET line = VEC 256 / bytesperword
  { readline("user> ", line)
    writes(rep(line))
  } REPEAT
}

LET start() = VALOF
{ LET ch = 0
  ch := rdch() REPEATUNTIL ch = '*n' // Consume command-line args
  wrch('*n') // Terminate prompt printed by Cintsys
  repl()
  RESULTIS 0
}
