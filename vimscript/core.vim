" core module

function MalEqualQ(args)
  return BoolNew(EqualQ(a:args[0], a:args[1]))
endfunction

function MalLt(args)
  return BoolNew(ObjValue(a:args[0]) < ObjValue(a:args[1]))
endfunction

function MalLte(args)
  return BoolNew(ObjValue(a:args[0]) <= ObjValue(a:args[1]))
endfunction

function MalGt(args)
  return BoolNew(ObjValue(a:args[0]) > ObjValue(a:args[1]))
endfunction

function MalGte(args)
  return BoolNew(ObjValue(a:args[0]) >= ObjValue(a:args[1]))
endfunction

function MalAdd(args)
  return IntegerNew(ObjValue(a:args[0]) + ObjValue(a:args[1]))
endfunction

function MalSub(args)
  return IntegerNew(ObjValue(a:args[0]) - ObjValue(a:args[1]))
endfunction

function MalMul(args)
  return IntegerNew(ObjValue(a:args[0]) * ObjValue(a:args[1]))
endfunction

function MalDiv(args)
  return IntegerNew(ObjValue(a:args[0]) / ObjValue(a:args[1]))
endfunction

function MalTimeMs(args)
  " vimtimems() is implemented in vimextras.c
  return IntegerNew(libcallnr("libvimextras.so", "vimtimems", 0))
endfunction

function MalList(args)
  return ListNew(a:args)
endfunction

function MalListQ(args)
  return BoolNew(ListQ(a:args[0]))
endfunction

function MalVector(args)
  return VectorNew(a:args)
endfunction

function MalVectorQ(args)
  return BoolNew(VectorQ(a:args[0]))
endfunction

function MalSequentialQ(args)
  return BoolNew(SequentialQ(a:args[0]))
endfunction

function MalHashMap(args)
  return HashBuild(a:args)
endfunction

function MalMapQ(args)
  return BoolNew(HashQ(a:args[0]))
endfunction

function MalEmptyQ(args)
  return BoolNew(EmptyQ(a:args[0]))
endfunction

function MalCount(args)
  return IntegerNew(ListCount(a:args[0]))
endfunction

function MalAssoc(args)
  let hash = copy(ObjValue(a:args[0]))
  let new_elements = HashBuild(a:args[1:])
  call extend(hash, ObjValue(new_elements))
  return HashNew(hash)
endfunction

function MalDissoc(args)
  let hash = copy(ObjValue(a:args[0]))
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
  let hash = ObjValue(a:args[0])
  let key = HashMakeKey(a:args[1])
  return get(hash, key, g:MalNil)
endfunction

function MalContainsQ(args)
  if !HashQ(a:args[0])
    return FalseNew()
  endif
  let hash = ObjValue(a:args[0])
  let key = HashMakeKey(a:args[1])
  return BoolNew(has_key(hash, key))
endfunction

function MalKeys(args)
  let listobjs = []
  for keyname in keys(ObjValue(a:args[0]))
    let keyobj = HashParseKey(keyname)
    call add(listobjs, keyobj)
  endfor
  return ListNew(listobjs)
endfunction

function MalVals(args)
  return ListNew(values(ObjValue(a:args[0])))
endfunction

function MalPrStr(args)
  return StringNew(join(map(copy(a:args), 'PrStr(v:val, 1)'), " "))
endfunction

function MalStr(args)
  return StringNew(join(map(copy(a:args), 'PrStr(v:val, 0)'), ""))
endfunction

function MalPrn(args)
  call PrintLn(join(map(copy(a:args), 'PrStr(v:val, 1)'), " "))
  return g:MalNil
endfunction

function MalPrintLn(args)
  call PrintLn(join(map(copy(a:args), 'PrStr(v:val, 0)'), " "))
  return g:MalNil
endfunction

function MalReadString(args)
  return ReadStr(ObjValue(a:args[0]))
endfunction

function MalReadLine(args)
  let [eof, line] = Readline(ObjValue(a:args[0]))
  return eof ? g:MalNil : StringNew(line)
endfunction

function MalSlurp(args)
  let filename = ObjValue(a:args[0])
  let lines = readfile(filename, "b")
  return StringNew(join(lines, "\n"))
endfunction

function MalCons(args)
  let items = copy(ObjValue(a:args[1]))
  call insert(items, a:args[0])
  return ListNew(items)
endfunction

function MalConcat(args)
  let res = []
  for list in a:args
    let res = res + ObjValue(list)
  endfor
  return ListNew(res)
endfunction

function MalFirst(args)
  return NilQ(a:args[0]) ? g:MalNil : ListFirst(a:args[0])
endfunction

function MalNth(args)
  return ListNth(a:args[0], ObjValue(a:args[1]))
endfunction

function MalRest(args)
  return NilQ(a:args[0]) ? ListNew([]) : ListRest(a:args[0])
endfunction

function MalApply(args)
  let funcobj = a:args[0]
  let rest = a:args[1:]
  if len(rest) == 0
    let funcargs = []
  elseif len(rest) == 1
    let funcargs = ObjValue(rest[-1])
  else
    let funcargs = rest[:-2] + ObjValue(rest[-1])
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
  for item in ObjValue(a:args[1])
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

function MalNilQ(args)
  return BoolNew(NilQ(a:args[0]))
endfunction

function MalTrueQ(args)
  return BoolNew(TrueQ(a:args[0]))
endfunction

function MalFalseQ(args)
  return BoolNew(FalseQ(a:args[0]))
endfunction

function MalSymbol(args)
  return SymbolNew(ObjValue(a:args[0]))
endfunction

function MalSymbolQ(args)
  return BoolNew(SymbolQ(a:args[0]))
endfunction

function MalStringQ(args)
  return BoolNew(StringQ(a:args[0]))
endfunction

function MalKeyword(args)
  return KeywordNew(ObjValue(a:args[0]))
endfunction

function MalKeywordQ(args)
  return BoolNew(KeywordQ(a:args[0]))
endfunction

function ConjList(list, elements)
  let newlist = a:list
  for e in a:elements
    let newlist = MalCons([e, newlist])
  endfor
  return newlist
endfunction

function ConjVector(vector, elements)
  let items = copy(ObjValue(a:vector))
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
    return ListNew(ObjValue(obj))
  elseif StringQ(obj)
    return ListNew(map(split(ObjValue(obj), '\zs'), 'StringNew(v:val)'))
  endif
  throw "seq requires string or list or vector or nil"
endfunction

function MalMeta(args)
  return ObjMeta(a:args[0])
endfunction

function MalWithMeta(args)
  let obj = a:args[0]
  return ObjNewWithMeta(ObjType(obj), copy(ObjValue(obj)), a:args[1])
endfunction

function MalAtom(args)
  return AtomNew(a:args[0])
endfunction

function MalAtomQ(args)
  return BoolNew(AtomQ(a:args[0]))
endfunction

function MalDeref(args)
  return ObjValue(a:args[0])
endfunction

function MalResetBang(args)
  return ObjSetValue(a:args[0], a:args[1])
endfunction

function MalSwapBang(args)
  let atomval = ObjValue(a:args[0])
  let funcobj = a:args[1]
  let args = a:args[2:]
  let res = MalApply([funcobj, ListNew([atomval] + args)])
  return ObjSetValue(a:args[0], res)
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

function MalVimStar(args)
  let vimexpr = ObjValue(a:args[0])
  let vimres = eval(vimexpr)
  return VimToMal(vimres)
endfunction

let CoreNs = {
  \ "=":           NewNativeFn("MalEqualQ"),
  \ "<":           NewNativeFn("MalLt"),
  \ "<=":          NewNativeFn("MalLte"),
  \ ">":           NewNativeFn("MalGt"),
  \ ">=":          NewNativeFn("MalGte"),
  \ "+":           NewNativeFn("MalAdd"),
  \ "-":           NewNativeFn("MalSub"),
  \ "*":           NewNativeFn("MalMul"),
  \ "/":           NewNativeFn("MalDiv"),
  \ "time-ms":     NewNativeFn("MalTimeMs"),
  \ "nil?":        NewNativeFn("MalNilQ"),
  \ "true?":       NewNativeFn("MalTrueQ"),
  \ "false?":      NewNativeFn("MalFalseQ"),
  \ "symbol":      NewNativeFn("MalSymbol"),
  \ "symbol?":     NewNativeFn("MalSymbolQ"),
  \ "string?":     NewNativeFn("MalStringQ"),
  \ "keyword":     NewNativeFn("MalKeyword"),
  \ "keyword?":    NewNativeFn("MalKeywordQ"),
  \ "list":        NewNativeFn("MalList"),
  \ "list?":       NewNativeFn("MalListQ"),
  \ "vector":      NewNativeFn("MalVector"),
  \ "vector?":     NewNativeFn("MalVectorQ"),
  \ "sequential?": NewNativeFn("MalSequentialQ"),
  \ "hash-map":    NewNativeFn("MalHashMap"),
  \ "map?":        NewNativeFn("MalMapQ"),
  \ "empty?":      NewNativeFn("MalEmptyQ"),
  \ "count":       NewNativeFn("MalCount"),
  \ "assoc":       NewNativeFn("MalAssoc"),
  \ "dissoc":      NewNativeFn("MalDissoc"),
  \ "get":         NewNativeFn("MalGet"),
  \ "contains?":   NewNativeFn("MalContainsQ"),
  \ "keys":        NewNativeFn("MalKeys"),
  \ "vals":        NewNativeFn("MalVals"),
  \ "pr-str":      NewNativeFn("MalPrStr"),
  \ "str":         NewNativeFn("MalStr"),
  \ "prn":         NewNativeFn("MalPrn"),
  \ "println":     NewNativeFn("MalPrintLn"),
  \ "read-string": NewNativeFn("MalReadString"),
  \ "readline":    NewNativeFn("MalReadLine"),
  \ "slurp":       NewNativeFn("MalSlurp"),
  \ "cons":        NewNativeFn("MalCons"),
  \ "concat":      NewNativeFn("MalConcat"),
  \ "first":       NewNativeFn("MalFirst"),
  \ "nth":         NewNativeFn("MalNth"),
  \ "rest":        NewNativeFn("MalRest"),
  \ "apply":       NewNativeFn("MalApply"),
  \ "map":         NewNativeFn("MalMap"),
  \ "throw":       NewNativeFn("MalThrow"),
  \ "conj":        NewNativeFn("MalConj"),
  \ "seq":         NewNativeFn("MalSeq"),
  \ "meta":        NewNativeFn("MalMeta"),
  \ "with-meta":   NewNativeFn("MalWithMeta"),
  \ "atom":        NewNativeFn("MalAtom"),
  \ "atom?":       NewNativeFn("MalAtomQ"),
  \ "deref":       NewNativeFn("MalDeref"),
  \ "reset!":      NewNativeFn("MalResetBang"),
  \ "swap!":       NewNativeFn("MalSwapBang"),
  \ "vim*":        NewNativeFn("MalVimStar")
  \ }
