" env module

let Env = {}

function NewEnv(outer)
  let e = copy(g:Env)
  let e.data = {}
  let e.outer = a:outer
  return e
endfunction

function NewEnvWithBinds(outer, binds, exprs)
  let env = NewEnv(a:outer)
  let i = 0
  while i < ListCount(a:binds)
    let varname = ObjValue(ListNth(a:binds, i))
    if varname == "&"
      " TODO
      let restvarname = ObjValue(ListNth(a:binds, i + 1))
      let restvarvalues = ListDrop(a:exprs, i)
      call env.set(restvarname, restvarvalues)
      break
    else
      unlet! varvalue
      let varvalue = ListNth(a:exprs, i)
      call env.set(varname, varvalue)
    endif
    let i = i + 1
  endwhile
  return env
endfunction

function Env.find(key) dict
  if has_key(self.data, a:key)
    return self
  elseif empty(self.outer)
    return ""
  else
    return self.outer.find(a:key)
  endif
endfunction

function Env.set(key, value) dict
  let self.data[a:key] = a:value
  return a:value
endfunction

function Env.get(key) dict
  let env = self.find(a:key)
  if empty(env)
    throw "'" . a:key . "' not found"
  endif
  return env.data[a:key]
endfunction

function Env.root() dict
  let curr = self
  while !empty(curr.outer)
    let curr = curr.outer
  endwhile
  return curr
endfunction
