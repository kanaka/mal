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
    let varname = a:ast.val
    return a:env.get(varname)
  elseif ListQ(a:ast)
    return ListNew(map(copy(a:ast.val), {_, e -> EVAL(e, a:env)}))
  elseif VectorQ(a:ast)
    return VectorNew(map(copy(a:ast.val), {_, e -> EVAL(e, a:env)}))
  elseif HashQ(a:ast)
    let ret = {}
    for [k,v] in items(a:ast.val)
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

  let first_symbol = a:ast.val[0].val
  if first_symbol == "def!"
    let a1 = a:ast.val[1]
    let a2 = a:ast.val[2]
    return a:env.set(a1.val, EVAL(a2, a:env))
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
  else
    " apply list
    let el = EvalAst(a:ast, a:env)
    let Fn = el.val[0]
    return Fn(el.val[1:-1])
  endif

endfunction

function PRINT(exp)
  return PrStr(a:exp, 1)
endfunction

function REP(str, env)
  return PRINT(EVAL(READ(a:str), a:env))
endfunction

let repl_env = NewEnv("")
call repl_env.set("+", {a -> IntegerNew(a[0].val + a[1].val)})
call repl_env.set("-", {a -> IntegerNew(a[0].val - a[1].val)})
call repl_env.set("*", {a -> IntegerNew(a[0].val * a[1].val)})
call repl_env.set("/", {a -> IntegerNew(a[0].val / a[1].val)})

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
