" types module

function ObjNewWithMeta(obj_type, obj_val, obj_meta)
  return {"type": a:obj_type, "val": a:obj_val, "meta": a:obj_meta}
endfunction

function ObjNew(obj_type, obj_val)
  return {"type": a:obj_type, "val": a:obj_val}
endfunction

function ObjType(obj)
  return a:obj["type"]
endfunction

function ObjValue(obj)
  return a:obj["val"]
endfunction

function ObjHasMeta(obj)
  return ObjQ(a:obj) && has_key(a:obj, "meta")
endfunction

function ObjMeta(obj)
  return ObjHasMeta(a:obj) ? a:obj["meta"] : g:MalNil
endfunction

function ObjSetValue(obj, newval)
  let a:obj["val"] = a:newval
  return a:newval
endfunction

function ObjSetMeta(obj, newmeta)
  let a:obj["meta"] = a:newmeta
  return a:newmeta
endfunction

function ObjQ(obj)
  return type(a:obj) == type({})
endfunction

function SymbolQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "symbol"
endfunction

function StringQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "string"
endfunction

function KeywordQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "keyword"
endfunction

function AtomQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "atom"
endfunction

function NilQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "nil"
endfunction

function TrueQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "true"
endfunction

function FalseQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "false"
endfunction

function IntegerQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "integer"
endfunction

function FloatQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "float"
endfunction

function ListQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "list"
endfunction

function VectorQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "vector"
endfunction

function SequentialQ(obj)
  return ObjQ(a:obj) && ListQ(a:obj) || VectorQ(a:obj)
endfunction

function HashQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "hash"
endfunction

function FunctionQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "function" && !ObjValue(a:obj).is_macro
endfunction

function MacroQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "function" && ObjValue(a:obj).is_macro
endfunction

function NativeFunctionQ(obj)
  return ObjQ(a:obj) && ObjType(a:obj) == "nativefunction"
endfunction

function NilNew()
  return ObjNew("nil", "")
endfunction

function TrueNew()
  return ObjNew("true", "")
endfunction

function FalseNew()
  return ObjNew("false", "")
endfunction

function BoolNew(bool)
  return a:bool ? g:MalTrue : g:MalFalse
endfunction

function KeywordNew(val)
  return ObjNew("keyword", a:val)
endfunction

function AtomNew(val)
  return ObjNewWithMeta("atom", a:val, g:MalNil)
endfunction

function SymbolNew(val)
  return ObjNew("symbol", a:val)
endfunction

function StringNew(val)
  return ObjNew("string", a:val)
endfunction

function IntegerNew(val)
  return ObjNew("integer", a:val)
endfunction

function FloatNew(val)
  return ObjNew("float", a:val)
endfunction

function ListNew(val)
  return ObjNewWithMeta("list", a:val, g:MalNil)
endfunction

function VectorNew(val)
  return ObjNewWithMeta("vector", a:val, g:MalNil)
endfunction

function HashNew(val)
  return ObjNewWithMeta("hash", a:val, g:MalNil)
endfunction

function HashMakeKey(obj)
  if !StringQ(a:obj) && !KeywordQ(a:obj)
    throw "expected hash-map key string, got: " . ObjType(a:obj));
  endif
  return ObjType(a:obj) . "#" . ObjValue(a:obj)
endfunction

function HashParseKey(str)
  if a:str =~ "^string#"
    return StringNew(a:str[7:])
  elseif a:str =~ "^keyword#"
    return KeywordNew(a:str[8:])
  endif
endfunction

function HashBuild(elements)
  if (len(a:elements) % 2) != 0
    throw "Odd number of hash-map arguments"
  endif
  let i = 0
  let hash = {}
  while i < len(a:elements)
    let key = a:elements[i]
    let val = a:elements[i + 1]
    let keystring = HashMakeKey(key)
    let hash[keystring] = val
    let i = i + 2
  endwhile
  return HashNew(hash)
endfunction

function HashEqualQ(x, y)
  if len(ObjValue(a:x)) != len(ObjValue(a:y))
    return 0
  endif
  for k in keys(ObjValue(a:x))
    let vx = ObjValue(a:x)[k]
    let vy = ObjValue(a:y)[k]
    if empty(vy) || !EqualQ(vx, vy)
      return 0
    endif
  endfor
  return 1
endfunction

function SequentialEqualQ(x, y)
  if len(ObjValue(a:x)) != len(ObjValue(a:y))
    return 0
  endif
  let i = 0
  while i < len(ObjValue(a:x))
    let ex = ObjValue(a:x)[i]
    let ey = ObjValue(a:y)[i]
    if !EqualQ(ex, ey)
      return 0
    endif
    let i = i +1
  endwhile
  return 1
endfunction

function EqualQ(x, y)
  if SequentialQ(a:x) && SequentialQ(a:y)
    return SequentialEqualQ(a:x, a:y)
  elseif HashQ(a:x) && HashQ(a:y)
    return HashEqualQ(a:x, a:y)
  elseif ObjType(a:x) != ObjType(a:y)
    return 0
  else
    return ObjValue(a:x) == ObjValue(a:y)
  endif
endfunction

function EmptyQ(list)
  return empty(ObjValue(a:list))
endfunction

function ListCount(list)
  return len(ObjValue(a:list))
endfunction

function ListNth(list, index)
  if a:index >= len(ObjValue(a:list))
    throw "nth: index out of range"
  endif
  return ObjValue(a:list)[a:index]
endfunction

function ListFirst(list)
  return get(ObjValue(a:list), 0, g:MalNil)
endfunction

function ListDrop(list, drop_elements)
  return ListNew(ObjValue(a:list)[a:drop_elements :])
endfunction

function ListRest(list)
  return ListDrop(a:list, 1)
endfunction

function FuncInvoke(funcobj, args)
  let fn = ObjValue(a:funcobj)
  let funcenv = NewEnvWithBinds(fn.env, fn.params, a:args)
  return EVAL(fn.ast, funcenv)
endfunction

function NativeFuncInvoke(funcobj, argslist)
  let fn = ObjValue(a:funcobj)
  return fn.Func(ObjValue(a:argslist))
endfunction

function MarkAsMacro(funcobj)
  let fn = ObjValue(a:funcobj)
  let fn.is_macro = 1
  return a:funcobj
endfunction

function NewFn(ast, env, params)
  let fn = {"ast": a:ast, "env": a:env, "params": a:params, "is_macro": 0}
  return ObjNewWithMeta("function", fn, g:MalNil)
endfunction

function NewNativeFn(funcname)
  let fn = {"Func": function(a:funcname), "name": a:funcname}
  return ObjNewWithMeta("nativefunction", fn, g:MalNil)
endfunction

let g:MalNil = NilNew()
let g:MalTrue = TrueNew()
let g:MalFalse = FalseNew()
