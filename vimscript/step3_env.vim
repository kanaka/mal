source readline.vim
source types.vim
source reader.vim
source printer.vim
source env.vim

function READ(str)
  return ReadStr(a:str)
endfunction

function EvalAst(ast, env)
  if SymbolQ(a:ast)
    let varname = ObjValue(a:ast)
    return a:env.get(varname)
  elseif ListQ(a:ast)
    let ret = []
    for e in ObjValue(a:ast)
      call add(ret, EVAL(e, a:env))
    endfor
    return ListNew(ret)
  elseif VectorQ(a:ast)
    let ret = []
    for e in ObjValue(a:ast)
      call add(ret, EVAL(e, a:env))
    endfor
    return VectorNew(ret)
  elseif HashQ(a:ast)
    let ret = {}
    for [k,v] in items(ObjValue(a:ast))
      let keyobj = HashParseKey(k)
      let newkey = EVAL(keyobj, a:env)
      let newval = EVAL(v, a:env)
      let keystring = HashMakeKey(newkey)
      let ret[keystring] = newval
    endfor
    return HashNew(ret)
  else
    return a:ast
  end
endfunction

function EVAL(ast, env)
  if !ListQ(a:ast)
    return EvalAst(a:ast, a:env)
  end
  if EmptyQ(a:ast)
    return a:ast
  endif

  let first_symbol = ObjValue(ObjValue(a:ast)[0])
  if first_symbol == "def!"
    let a1 = ObjValue(a:ast)[1]
    let a2 = ObjValue(a:ast)[2]
    return a:env.set(ObjValue(a1), EVAL(a2, a:env))
  elseif first_symbol == "let*"
    let a1 = ObjValue(a:ast)[1]
    let a2 = ObjValue(a:ast)[2]
    let let_env = NewEnv(a:env)
    let let_binds = ObjValue(a1)
    let i = 0
    while i < len(let_binds)
      call let_env.set(ObjValue(let_binds[i]), EVAL(let_binds[i+1], let_env))
      let i = i + 2
    endwhile
    return EVAL(a2, let_env)
  else
    " apply list
    let el = EvalAst(a:ast, a:env)
    let Fn = ObjValue(el)[0]
    return Fn(ObjValue(el)[1:-1])
  endif

endfunction

function PRINT(exp)
  return PrStr(a:exp, 1)
endfunction

function REP(str, env)
  return PRINT(EVAL(READ(a:str), a:env))
endfunction

function MalAdd(args)
  return IntegerNew(ObjValue(a:args[0]) + ObjValue(a:args[1]))
endfunction

function MalSub(args)
  return IntegerNew(ObjValue(a:args[0]) - ObjValue(a:args[1]))
endfunction

function MalMul(args)
  return IntegerNew(ObjValue(a:args[0]) * ObjValue(a:args[1]))
endfunction

function MalDiv(args)
  return IntegerNew(ObjValue(a:args[0]) / ObjValue(a:args[1]))
endfunction

let repl_env = NewEnv("")
call repl_env.set("+", function("MalAdd"))
call repl_env.set("-", function("MalSub"))
call repl_env.set("*", function("MalMul"))
call repl_env.set("/", function("MalDiv"))

while 1
  let [eof, line] = Readline("user> ")
  if eof
    break
  endif
  if line == ""
    continue
  endif
  try
    call PrintLn(REP(line, repl_env))
  catch
    call PrintLn("Error: " . v:exception)
  endtry
endwhile
qall!
