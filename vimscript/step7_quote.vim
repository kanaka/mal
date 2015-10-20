source readline.vim
source types.vim
source reader.vim
source printer.vim
source env.vim
source core.vim

function READ(str)
  return ReadStr(a:str)
endfunction

function PairQ(obj)
  return SequentialQ(a:obj) && !EmptyQ(a:obj)
endfunction

function Quasiquote(ast)
  if !PairQ(a:ast)
    return ListNew([SymbolNew("quote"), a:ast])
  endif
  let a0 = ListFirst(a:ast)
  if SymbolQ(a0) && ObjValue(a0) == "unquote"
    return ListNth(a:ast, 1)
  elseif PairQ(a0) && SymbolQ(ListFirst(a0)) && ObjValue(ListFirst(a0)) == "splice-unquote"
    return ListNew([SymbolNew("concat"), ListNth(a0, 1), Quasiquote(ListRest(a:ast))])
  else
    return ListNew([SymbolNew("cons"), Quasiquote(a0), Quasiquote(ListRest(a:ast))])
  end
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
  let ast = a:ast
  let env = a:env

  while 1
    if !ListQ(ast)
      return EvalAst(ast, env)
    end

    let first = ListFirst(ast)
    let first_symbol = SymbolQ(first) ? ObjValue(first) : ""
    if first_symbol == "def!"
      let a1 = ObjValue(ast)[1]
      let a2 = ObjValue(ast)[2]
      let ret = env.set(ObjValue(a1), EVAL(a2, env))
      return ret
    elseif first_symbol == "let*"
      let a1 = ObjValue(ast)[1]
      let a2 = ObjValue(ast)[2]
      let env = NewEnv(env)
      let let_binds = ObjValue(a1)
      let i = 0
      while i < len(let_binds)
        call env.set(ObjValue(let_binds[i]), EVAL(let_binds[i+1], env))
        let i = i + 2
      endwhile
      let ast = a2
      " TCO
    elseif first_symbol == "quote"
      return ListNth(ast, 1)
    elseif first_symbol == "quasiquote"
      let ast = Quasiquote(ListNth(ast, 1))
      " TCO
    elseif first_symbol == "if"
      let condvalue = EVAL(ObjValue(ast)[1], env)
      if FalseQ(condvalue) || NilQ(condvalue)
        if len(ObjValue(ast)) < 4
          return g:MalNil
        else
          let ast = ObjValue(ast)[3]
        endif
      else
        let ast = ObjValue(ast)[2]
      endif
      " TCO
    elseif first_symbol == "do"
      let astlist = ObjValue(ast)
      call EvalAst(ListNew(astlist[1:-2]), env)
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
      let el = EvalAst(ast, env)
      let funcobj = ListFirst(el)
      let args = ListRest(el)
      if NativeFunctionQ(funcobj)
        return NativeFuncInvoke(funcobj, args)
      elseif FunctionQ(funcobj)
        let fn = ObjValue(funcobj)
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
  let args = argv()
  let list = []
  for arg in args[1:]
    call add(list, StringNew(arg))
  endfor
  return ListNew(list)
endfunction

set maxfuncdepth=10000
let repl_env = NewEnv("")

for [k, v] in items(CoreNs)
  call repl_env.set(k, v)
endfor

call repl_env.set("*ARGV*", GetArgvList())

call RE("(def! not (fn* (a) (if a false true)))", repl_env)
call RE("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))", repl_env)

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
