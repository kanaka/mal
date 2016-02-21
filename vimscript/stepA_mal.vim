source readline.vim
source types.vim
source reader.vim
source printer.vim
source env.vim
source core.vim

let MalExceptionObj = ""

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

function IsMacroCall(ast, env)
  if !ListQ(a:ast)
    return 0
  endif
  let a0 = ListFirst(a:ast)
  if !SymbolQ(a0)
    return 0
  endif
  let macroname = ObjValue(a0)
  if empty(a:env.find(macroname))
    return 0
  endif
  return MacroQ(a:env.get(macroname))
endfunction

function MacroExpand(ast, env)
  let ast = a:ast
  while IsMacroCall(ast, a:env)
    let macroobj = a:env.get(ObjValue(ListFirst(ast)))
    let macroargs = ListRest(ast)
    let ast = FuncInvoke(macroobj, macroargs)
  endwhile
  return ast
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

function GetCatchClause(ast)
  if ListCount(a:ast) < 3
    return ""
  end
  let catch_clause = ListNth(a:ast, 2)
  if ListFirst(catch_clause) == SymbolNew("catch*")
    return catch_clause
  else
    return ""
  end
endfunction

function EVAL(ast, env)
  let ast = a:ast
  let env = a:env

  while 1
    if !ListQ(ast)
      return EvalAst(ast, env)
    end

    let ast = MacroExpand(ast, env)
    if !ListQ(ast)
      return EvalAst(ast, env)
    end

    let first = ListFirst(ast)
    let first_symbol = SymbolQ(first) ? ObjValue(first) : ""
    if first_symbol == "def!"
      let a1 = ObjValue(ast)[1]
      let a2 = ObjValue(ast)[2]
      return env.set(ObjValue(a1), EVAL(a2, env))
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
    elseif first_symbol == "defmacro!"
      let a1 = ListNth(ast, 1)
      let a2 = ListNth(ast, 2)
      let macro = MarkAsMacro(EVAL(a2, env))
      return env.set(ObjValue(a1), macro)
    elseif first_symbol == "macroexpand"
      return MacroExpand(ListNth(ast, 1), env)
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
    elseif first_symbol == "try*"
      try
        return EVAL(ListNth(ast, 1), env)
      catch
        let catch_clause = GetCatchClause(ast)
        if empty(catch_clause)
          throw v:exception
        endif

        let exc_var = ObjValue(ListNth(catch_clause, 1))
        if v:exception == "__MalException__"
          let exc_value = g:MalExceptionObj
        else
          let exc_value = StringNew(v:exception)
        endif
        let catch_env = NewEnvWithBinds(env, ListNew([SymbolNew(exc_var)]), ListNew([exc_value]))
        return EVAL(ListNth(catch_clause, 2), catch_env)
      endtry
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

call RE("(def! *host-language* \"vimscript\")", repl_env)
call RE("(def! not (fn* (a) (if a false true)))", repl_env)
call RE("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))", repl_env)
call RE("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))", repl_env)
call RE("(def! *gensym-counter* (atom 0))", repl_env)
call RE("(def! gensym (fn* [] (symbol (str \"G__\" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))", repl_env)
call RE("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs)) (if ~condvar ~condvar (or ~@(rest xs)))))))))", repl_env)

if !empty(argv())
  try
    call RE('(load-file "' . argv(0) . '")', repl_env)
  catch
    call PrintLn("Error: " . v:exception)
  endtry
  qall!
endif

call REP("(println (str \"Mal [\" *host-language* \"]\"))", repl_env)

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
