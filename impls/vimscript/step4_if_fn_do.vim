source readline.vim
source types.vim
source reader.vim
source printer.vim
source env.vim
source core.vim

function READ(str)
  return ReadStr(a:str)
endfunction

function EVAL(ast, env)
  let dbgeval = a:env.get("DEBUG-EVAL")
  if !(empty(dbgeval) || FalseQ(dbgeval) || NilQ(dbgeval))
    call PrintLn("EVAL: " . PrStr(a:ast, 1))
  endif

  if SymbolQ(a:ast)
    let varname = a:ast.val
    let val = a:env.get(varname)
    if empty(val)
      throw "'" . varname . "' not found"
    endif
    return val
  elseif VectorQ(a:ast)
    return VectorNew(map(copy(a:ast.val), {_, e -> EVAL(e, a:env)}))
  elseif HashQ(a:ast)
    let ret = {}
    for [k,v] in items(a:ast.val)
      let newval = EVAL(v, a:env)
      let ret[k] = newval
    endfor
    return HashNew(ret)
  endif
  if !ListQ(a:ast)
    return a:ast
  end
  if EmptyQ(a:ast)
    return a:ast
  endif

  let first = ListFirst(a:ast)
  let first_symbol = SymbolQ(first) ? first.val : ""
  if first_symbol == "def!"
    let a1 = a:ast.val[1]
    let a2 = a:ast.val[2]
    let ret = a:env.set(a1.val, EVAL(a2, a:env))
    return ret
  elseif first_symbol == "let*"
    let a1 = a:ast.val[1]
    let a2 = a:ast.val[2]
    let let_env = NewEnv(a:env)
    let let_binds = a1.val
    let i = 0
    while i < len(let_binds)
      call let_env.set(let_binds[i].val, EVAL(let_binds[i+1], let_env))
      let i = i + 2
    endwhile
    return EVAL(a2, let_env)
  elseif first_symbol == "if"
    let condvalue = EVAL(a:ast.val[1], a:env)
    if FalseQ(condvalue) || NilQ(condvalue)
      if len(a:ast.val) < 4
        return g:MalNil
      else
        return EVAL(a:ast.val[3], a:env)
      endif
    else
      return EVAL(a:ast.val[2], a:env)
    endif
  elseif first_symbol == "do"
    let astlist = a:ast.val
    for elt in astlist[1:-2]
      let ignored = EVAL(elt, a:env)
    endfor
    return EVAL(astlist[-1], a:env)
  elseif first_symbol == "fn*"
    let fn = NewFn(ListNth(a:ast, 2), a:env, ListNth(a:ast, 1))
    return fn
  else
    " apply list
    let el = ListNew(map(copy(a:ast.val), {_, e -> EVAL(e, a:env)}))
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
