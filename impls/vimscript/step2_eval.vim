source readline.vim
source types.vim
source reader.vim
source printer.vim

function READ(str)
  return ReadStr(a:str)
endfunction

function EVAL(ast, env)
  " call PrintLn("EVAL: " . PrStr(a:ast, 1))

  if SymbolQ(a:ast)
    let varname = a:ast.val
    if !has_key(a:env, varname)
      throw "'" . varname . "' not found"
    end
    return a:env[varname]
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

  " apply list
  let el = ListNew(map(copy(a:ast.val), {_, e -> EVAL(e, a:env)}))

  let Fn = el.val[0]
  return Fn(el.val[1:-1])
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
