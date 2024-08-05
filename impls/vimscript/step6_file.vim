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
  let ast = a:ast
  let env = a:env

  while 1

    let dbgeval = env.get("DEBUG-EVAL")
    if !(empty(dbgeval) || FalseQ(dbgeval) || NilQ(dbgeval))
      call PrintLn("EVAL: " . PrStr(ast, 1))
    endif

    if SymbolQ(ast)
      let varname = ast.val
      let val = env.get(varname)
      if empty(val)
        throw "'" . varname . "' not found"
      endif
      return val
    elseif VectorQ(ast)
      return VectorNew(map(copy(ast.val), {_, e -> EVAL(e, env)}))
    elseif HashQ(ast)
      let ret = {}
      for [k,v] in items(ast.val)
        let newval = EVAL(v, env)
        let ret[k] = newval
      endfor
      return HashNew(ret)
    endif
    if !ListQ(ast)
      return ast
    end
    if EmptyQ(ast)
      return ast
    endif

    let first = ListFirst(ast)
    let first_symbol = SymbolQ(first) ? first.val : ""
    if first_symbol == "def!"
      let a1 = ast.val[1]
      let a2 = ast.val[2]
      let ret = env.set(a1.val, EVAL(a2, env))
      return ret
    elseif first_symbol == "let*"
      let a1 = ast.val[1]
      let a2 = ast.val[2]
      let env = NewEnv(env)
      let let_binds = a1.val
      let i = 0
      while i < len(let_binds)
        call env.set(let_binds[i].val, EVAL(let_binds[i+1], env))
        let i = i + 2
      endwhile
      let ast = a2
      " TCO
    elseif first_symbol == "if"
      let condvalue = EVAL(ast.val[1], env)
      if FalseQ(condvalue) || NilQ(condvalue)
        if len(ast.val) < 4
          return g:MalNil
        else
          let ast = ast.val[3]
        endif
      else
        let ast = ast.val[2]
      endif
      " TCO
    elseif first_symbol == "do"
      let astlist = ast.val
      for elt in astlist[1:-2]
        let ignored = EVAL(elt, env)
      endfor
      let ast = astlist[-1]
      " TCO
    elseif first_symbol == "fn*"
      let fn = NewFn(ListNth(ast, 2), env, ListNth(ast, 1))
      return fn
    elseif first_symbol == "eval"
      let ast = EVAL(ListNth(ast, 1), env)
      let env = env.root()
      " TCO
    else
      " apply list
      let el = ListNew(map(copy(ast.val), {_, e -> EVAL(e, env)}))
      let funcobj = ListFirst(el)
      let args = ListRest(el)
      if NativeFunctionQ(funcobj)
        return NativeFuncInvoke(funcobj, args)
      elseif FunctionQ(funcobj)
        let fn = funcobj.val
        let ast = fn.ast
        let env = NewEnvWithBinds(fn.env, fn.params, args)
        " TCO
      else
        throw "Not a function"
      endif
    endif
  endwhile
endfunction

function PRINT(exp)
  return PrStr(a:exp, 1)
endfunction

function RE(str, env)
  return EVAL(READ(a:str), a:env)
endfunction

function REP(str, env)
  return PRINT(EVAL(READ(a:str), a:env))
endfunction

function GetArgvList()
  return ListNew(map(copy(argv()[1:]), {_, arg -> StringNew(arg)}))
endfunction

set maxfuncdepth=10000
let repl_env = NewEnv("")

for [k, v] in items(CoreNs)
  call repl_env.set(k, v)
endfor

call repl_env.set("*ARGV*", GetArgvList())

call RE("(def! not (fn* (a) (if a false true)))", repl_env)
call RE("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))", repl_env)

if !empty(argv())
  call RE('(load-file "' . argv(0) . '")', repl_env)
  qall!
endif

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
