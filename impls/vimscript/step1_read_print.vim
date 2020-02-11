source readline.vim
source types.vim
source reader.vim
source printer.vim

function READ(str)
  return ReadStr(a:str)
endfunction

function EVAL(ast, env)
  return a:ast
endfunction

function PRINT(exp)
  return PrStr(a:exp, 1)
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
  try
    call PrintLn(REP(line))
  catch
    call PrintLn("Error: " . v:exception)
  endtry
endwhile
qall!
