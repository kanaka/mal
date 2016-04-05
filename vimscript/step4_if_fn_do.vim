source readline.vim
source types.vim
source reader.vim
source printer.vim
source env.vim
source core.vim

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

  let first = ListFirst(a:ast)
  let first_symbol = SymbolQ(first) ? ObjValue(first) : ""
  if first_symbol == "def!"
    let a1 = ObjValue(a:ast)[1]
    let a2 = ObjValue(a:ast)[2]
    let ret = a:env.set(ObjValue(a1), EVAL(a2, a:env))
    return ret
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
  elseif first_symbol == "if"
    let condvalue = EVAL(ObjValue(a:ast)[1], a:env)
    if FalseQ(condvalue) || NilQ(condvalue)
      if len(ObjValue(a:ast)) < 4
        return g:MalNil
      else
        return EVAL(ObjValue(a:ast)[3], a:env)
      endif
    else
      return EVAL(ObjValue(a:ast)[2], a:env)
    endif
  elseif first_symbol == "do"
    let el = EvalAst(ListRest(a:ast), a:env)
    return ObjValue(el)[-1]
  elseif first_symbol == "fn*"
    let fn = NewFn(ListNth(a:ast, 2), a:env, ListNth(a:ast, 1))
    return fn
  else
    " apply list
    let el = EvalAst(a:ast, a:env)
    let funcobj = ListFirst(el)
    let args = ListRest(el)
    if NativeFunctionQ(funcobj)
      return NativeFuncInvoke(funcobj, args)
    elseif FunctionQ(funcobj)
      return FuncInvoke(funcobj, args)
    else
      throw "Not a function"
    endif
  endif
endfunction

function PRINT(exp)
  return PrStr(a:exp, 1)
endfunction

function REP(str, env)
  return PRINT(EVAL(READ(a:str), a:env))
endfunction

let repl_env = NewEnv("")

for [k, Fn] in items(CoreNs)
  call repl_env.set(k, Fn)
endfor

call REP("(def! not (fn* (a) (if a false true)))", repl_env)

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
