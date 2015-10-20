source readline.vim

function READ(str)
  return a:str
endfunction

function EVAL(ast, env)
  return a:ast
endfunction

function PRINT(exp)
  return a:exp
endfunction

function REP(str)
  return PRINT(EVAL(READ(a:str), {}))
endfunction

while 1
  let [eof, line] = Readline("user> ")
  if eof
    break
  endif
  if line == ""
    continue
  endif
  call PrintLn(REP(line))
endwhile
qall!
