source readline.vim
source types.vim
source reader.vim
source printer.vim

function READ(str)
  return ReadStr(a:str)
endfunction

function EvalAst(ast, env)
  if SymbolQ(a:ast)
    let varname = ObjValue(a:ast)
    if !has_key(a:env, varname)
      throw "'" . varname . "' not found"
    end
    return a:env[varname]
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

  " apply list
  let el = EvalAst(a:ast, a:env)

  let Fn = ObjValue(el)[0]
  return Fn(ObjValue(el)[1:-1])
endfunction

function PRINT(exp)
  return PrStr(a:exp, 1)
endfunction

function REP(str, env)
  return PRINT(EVAL(READ(a:str), a:env))
endfunction

let repl_env = {}
let repl_env["+"] = {a -> IntegerNew(a[0].val + a[1].val)}
let repl_env["-"] = {a -> IntegerNew(a[0].val - a[1].val)}
let repl_env["*"] = {a -> IntegerNew(a[0].val * a[1].val)}
let repl_env["/"] = {a -> IntegerNew(a[0].val / a[1].val)}

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
    call PrintLn("ERROR: " . v:exception)
  endtry
endwhile
qall!
