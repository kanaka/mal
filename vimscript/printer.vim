" printer module

function PrStr(ast, readable)
  let obj = a:ast
  let r = a:readable
  if ListQ(obj)
    let ret = []
    for e in obj.val
      call add(ret, PrStr(e, r))
    endfor
    return "(" . join(ret, " ") . ")"
  elseif VectorQ(obj)
    let ret = []
    for e in obj.val
      call add(ret, PrStr(e, r))
    endfor
    return "[" . join(ret, " ") . "]"
  elseif HashQ(obj)
    let ret = []
    for [k, v] in items(obj.val)
      let keyobj = HashParseKey(k)
      call add(ret, PrStr(keyobj, r))
      call add(ret, PrStr(v, r))
    endfor
    return "{" . join(ret, " ") . "}"
  elseif MacroQ(obj)
    let numargs = ListCount(obj.val.params)
    return "<Macro:" . numargs . "-arguments>"
  elseif FunctionQ(obj)
    let numargs = ListCount(obj.val.params)
    return "<Function:" . numargs . "-arguments>"
  elseif NativeFunctionQ(obj)
    let funcname = obj.val.name
    return "<NativeFunction:" . funcname . ">"
  elseif AtomQ(obj)
    return "(atom " . PrStr(obj.val, 1) . ")"
  elseif KeywordQ(obj)
    return ':' . obj.val
  elseif StringQ(obj)
    if r
      let str = obj.val
      let str = substitute(str, '\\', '\\\\', "g")
      let str = substitute(str, '"', '\\"', "g")
      let str = substitute(str, "\n", '\\n', "g")
      return '"' . str . '"'
    else
      return obj.val
    endif
  elseif NilQ(obj)
    return "nil"
  elseif TrueQ(obj)
    return "true"
  elseif FalseQ(obj)
    return "false"
  elseif IntegerQ(obj) || FloatQ(obj)
    return string(obj.val)
  else
    return obj.val
  end
endfunction
