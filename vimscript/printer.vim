" printer module

function PrStr(ast, readable)
  let obj = a:ast
  let r = a:readable
  if ListQ(obj)
    let ret = []
    for e in ObjValue(obj)
      call add(ret, PrStr(e, r))
    endfor
    return "(" . join(ret, " ") . ")"
  elseif VectorQ(obj)
    let ret = []
    for e in ObjValue(obj)
      call add(ret, PrStr(e, r))
    endfor
    return "[" . join(ret, " ") . "]"
  elseif HashQ(obj)
    let ret = []
    for [k, v] in items(ObjValue(obj))
      let keyobj = HashParseKey(k)
      call add(ret, PrStr(keyobj, r))
      call add(ret, PrStr(v, r))
    endfor
    return "{" . join(ret, " ") . "}"
  elseif MacroQ(obj)
    let numargs = ListCount(ObjValue(obj).params)
    return "<Macro:" . numargs . "-arguments>"
  elseif FunctionQ(obj)
    let numargs = ListCount(ObjValue(obj).params)
    return "<Function:" . numargs . "-arguments>"
  elseif NativeFunctionQ(obj)
    let funcname = ObjValue(obj).name
    return "<NativeFunction:" . funcname . ">"
  elseif AtomQ(obj)
    return "(atom " . PrStr(ObjValue(obj), 1) . ")"
  elseif KeywordQ(obj)
    return ':' . ObjValue(obj)
  elseif StringQ(obj)
    if r
      let str = ObjValue(obj)
      let str = substitute(str, '\\', '\\\\', "g")
      let str = substitute(str, '"', '\\"', "g")
      let str = substitute(str, "\n", '\\n', "g")
      return '"' . str . '"'
    else
      return ObjValue(obj)
    endif
  elseif NilQ(obj)
    return "nil"
  elseif TrueQ(obj)
    return "true"
  elseif FalseQ(obj)
    return "false"
  elseif IntegerQ(obj) || FloatQ(obj)
    return string(ObjValue(obj))
  else
    return ObjValue(obj)
  end
endfunction
