" core module

function MalAssoc(args)
  let hash = copy(a:args[0].val)
  let new_elements = HashBuild(a:args[1:])
  call extend(hash, new_elements.val)
  return HashNew(hash)
endfunction

function MalDissoc(args)
  let hash = copy(a:args[0].val)
  for keyobj in a:args[1:]
    let key = HashMakeKey(keyobj)
    if has_key(hash, key)
      call remove(hash, key)
    endif
  endfor
  return HashNew(hash)
endfunction

function MalGet(args)
  if !HashQ(a:args[0])
    return g:MalNil
  endif
  let hash = a:args[0].val
  let key = HashMakeKey(a:args[1])
  return get(hash, key, g:MalNil)
endfunction

function MalContainsQ(args)
  if !HashQ(a:args[0])
    return FalseNew()
  endif
  let hash = a:args[0].val
  let key = HashMakeKey(a:args[1])
  return BoolNew(has_key(hash, key))
endfunction

function MalKeys(args)
  let listobjs = []
  for keyname in keys(a:args[0].val)
    let keyobj = HashParseKey(keyname)
    call add(listobjs, keyobj)
  endfor
  return ListNew(listobjs)
endfunction

function MalReadLine(args)
  let [eof, line] = Readline(a:args[0].val)
  return eof ? g:MalNil : StringNew(line)
endfunction

function MalCons(args)
  let items = copy(a:args[1].val)
  call insert(items, a:args[0])
  return ListNew(items)
endfunction

function MalConcat(args)
  let res = []
  for list in a:args
    let res = res + list.val
  endfor
  return ListNew(res)
endfunction

function MalApply(args)
  let funcobj = a:args[0]
  let rest = a:args[1:]
  if len(rest) == 0
    let funcargs = []
  elseif len(rest) == 1
    let funcargs = rest[-1].val
  else
    let funcargs = rest[:-2] + rest[-1].val
  endif
  if NativeFunctionQ(funcobj)
    return NativeFuncInvoke(funcobj, ListNew(funcargs))
  elseif FunctionQ(funcobj)
    return FuncInvoke(funcobj, ListNew(funcargs))
  else
    throw "Not a function"
  endif
endfunction

function MalMap(args)
  let funcobj = a:args[0]
  let res = []
  for item in a:args[1].val
    unlet! mappeditem
    if NativeFunctionQ(funcobj)
      let mappeditem = NativeFuncInvoke(funcobj, ListNew([item]))
    elseif FunctionQ(funcobj)
      let mappeditem = FuncInvoke(funcobj, ListNew([item]))
    else
      throw "Not a function"
    endif
    call add(res, mappeditem)
  endfor
  return ListNew(res)
endfunction

function MalThrow(args)
  unlet! g:MalExceptionObj
  let g:MalExceptionObj = a:args[0]
  throw "__MalException__"
endfunction

function ConjList(list, elements)
  let newlist = a:list
  for e in a:elements
    let newlist = MalCons([e, newlist])
  endfor
  return newlist
endfunction

function ConjVector(vector, elements)
  let items = copy(a:vector.val)
  for e in a:elements
    call add(items, e)
  endfor
  return VectorNew(items)
endfunction

function MalConj(args)
  if ListQ(a:args[0])
    return ConjList(a:args[0], a:args[1:])
  elseif VectorQ(a:args[0])
    return ConjVector(a:args[0], a:args[1:])
  endif
endfunction

function MalSeq(args)
  let obj = a:args[0]
  if EmptyQ(obj)
    return g:MalNil
  elseif ListQ(obj)
    return obj
  elseif VectorQ(obj)
    return ListNew(obj.val)
  elseif StringQ(obj)
    return ListNew(map(split(obj.val, '\zs'), {_, c -> StringNew(c)}))
  endif
  throw "seq requires string or list or vector or nil"
endfunction

function VimToMal(e)
  if type(a:e) == type(0)
    return IntegerNew(a:e)
  elseif type(a:e) == type(0.0)
    return FloatNew(a:e)
  elseif type(a:e) == type("")
    return StringNew(a:e)
  elseif type(a:e) == type([])
    let res = []
    for v in a:e
      call add(res, VimToMal(v))
    endfor
    return ListNew(res)
  elseif type(a:e) == type({})
    let res = {}
    for [k,v] in items(a:e)
      let keystring = HashMakeKey(StringNew(k))
      let res[keystring] = VimToMal(v)
    endfor
    return HashNew(res)
  else
    return g:MalNil
  endif
endfunction

let CoreNs = {
  \ "=":           NewNativeFnLambda({a -> BoolNew(EqualQ(a[0], a[1]))}),
  \ "<":           NewNativeFnLambda({a -> BoolNew(a[0].val < a[1].val)}),
  \ "<=":          NewNativeFnLambda({a -> BoolNew(a[0].val <= a[1].val)}),
  \ ">":           NewNativeFnLambda({a -> BoolNew(a[0].val > a[1].val)}),
  \ ">=":          NewNativeFnLambda({a -> BoolNew(a[0].val >= a[1].val)}),
  \ "+":           NewNativeFnLambda({a -> IntegerNew(a[0].val + a[1].val)}),
  \ "-":           NewNativeFnLambda({a -> IntegerNew(a[0].val - a[1].val)}),
  \ "*":           NewNativeFnLambda({a -> IntegerNew(a[0].val * a[1].val)}),
  \ "/":           NewNativeFnLambda({a -> IntegerNew(a[0].val / a[1].val)}),
  \ "time-ms":     NewNativeFnLambda({a -> IntegerNew(libcallnr("libvimextras.so", "vimtimems", 0))}),
  \ "nil?":        NewNativeFnLambda({a -> BoolNew(NilQ(a[0]))}),
  \ "true?":       NewNativeFnLambda({a -> BoolNew(TrueQ(a[0]))}),
  \ "false?":      NewNativeFnLambda({a -> BoolNew(FalseQ(a[0]))}),
  \ "symbol":      NewNativeFnLambda({a -> SymbolNew(a[0].val)}),
  \ "symbol?":     NewNativeFnLambda({a -> BoolNew(SymbolQ(a[0]))}),
  \ "string?":     NewNativeFnLambda({a -> BoolNew(StringQ(a[0]))}),
  \ "keyword":     NewNativeFnLambda({a -> KeywordNew(a[0].val)}),
  \ "keyword?":    NewNativeFnLambda({a -> BoolNew(KeywordQ(a[0]))}),
  \ "number?":     NewNativeFnLambda({a -> BoolNew(IntegerQ(a[0]))}),
  \ "fn?":         NewNativeFnLambda({a -> BoolNew(NativeFunctionQ(a[0]) || FunctionQ(a[0]))}),
  \ "macro?":      NewNativeFnLambda({a -> BoolNew(MacroQ(a[0]))}),
  \ "list":        NewNativeFnLambda({a -> ListNew(a)}),
  \ "list?":       NewNativeFnLambda({a -> BoolNew(ListQ(a[0]))}),
  \ "vector":      NewNativeFnLambda({a -> VectorNew(a)}),
  \ "vector?":     NewNativeFnLambda({a -> BoolNew(VectorQ(a[0]))}),
  \ "sequential?": NewNativeFnLambda({a -> BoolNew(SequentialQ(a[0]))}),
  \ "hash-map":    NewNativeFnLambda({a -> HashBuild(a)}),
  \ "map?":        NewNativeFnLambda({a -> BoolNew(HashQ(a[0]))}),
  \ "empty?":      NewNativeFnLambda({a -> BoolNew(EmptyQ(a[0]))}),
  \ "count":       NewNativeFnLambda({a -> IntegerNew(ListCount(a[0]))}),
  \ "assoc":       NewNativeFn("MalAssoc"),
  \ "dissoc":      NewNativeFn("MalDissoc"),
  \ "get":         NewNativeFn("MalGet"),
  \ "contains?":   NewNativeFn("MalContainsQ"),
  \ "keys":        NewNativeFn("MalKeys"),
  \ "vals":        NewNativeFnLambda({a -> ListNew(values(a[0].val))}),
  \ "pr-str":      NewNativeFnLambda({a -> StringNew(join(map(copy(a), {_, e -> PrStr(e, 1)}), " "))}),
  \ "str":         NewNativeFnLambda({a -> StringNew(join(map(copy(a), {_, e -> PrStr(e, 0)}), ""))}),
  \ "prn":         NewNativeFnLambda({a -> [PrintLn(join(map(copy(a), {_, e -> PrStr(e, 1)}), " ")), g:MalNil][1]}),
  \ "println":     NewNativeFnLambda({a -> [PrintLn(join(map(copy(a), {_, e -> PrStr(e, 0)}), " ")), g:MalNil][1]}),
  \ "read-string": NewNativeFnLambda({a -> ReadStr(a[0].val)}),
  \ "readline":    NewNativeFn("MalReadLine"),
  \ "slurp":       NewNativeFnLambda({a -> StringNew(join(readfile(a[0].val, "b"), "\n"))}),
  \ "cons":        NewNativeFn("MalCons"),
  \ "concat":      NewNativeFn("MalConcat"),
  \ "vec":         NewNativeFnLambda({a -> VectorNew(a[0].val)}),
  \ "first":       NewNativeFnLambda({a -> NilQ(a[0]) ? g:MalNil : ListFirst(a[0])}),
  \ "nth":         NewNativeFnLambda({a -> ListNth(a[0], a[1].val)}),
  \ "rest":        NewNativeFnLambda({a -> NilQ(a[0]) ? ListNew([]) : ListRest(a[0])}),
  \ "apply":       NewNativeFn("MalApply"),
  \ "map":         NewNativeFn("MalMap"),
  \ "throw":       NewNativeFn("MalThrow"),
  \ "conj":        NewNativeFn("MalConj"),
  \ "seq":         NewNativeFn("MalSeq"),
  \ "meta":        NewNativeFnLambda({a -> ObjMeta(a[0])}),
  \ "with-meta":   NewNativeFnLambda({a -> ObjNewWithMeta(a[0].type, copy(a[0].val), a[1])}),
  \ "atom":        NewNativeFnLambda({a -> AtomNew(a[0])}),
  \ "atom?":       NewNativeFnLambda({a -> BoolNew(AtomQ(a[0]))}),
  \ "deref":       NewNativeFnLambda({a -> a[0].val}),
  \ "reset!":      NewNativeFnLambda({a -> ObjSetValue(a[0], a[1])}),
  \ "swap!":       NewNativeFnLambda({a -> ObjSetValue(a[0], MalApply([a[1], ListNew([a[0].val] + a[2:])]))}),
  \ "vim*":        NewNativeFnLambda({a -> VimToMal(eval(a[0].val))})
  \ }
