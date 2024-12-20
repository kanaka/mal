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
    let varname = ListNth(a:binds, i).val
    if varname == "&"
      let restvarname = ListNth(a:binds, i + 1).val
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

function Env.set(key, value) dict
  let self.data[a:key] = a:value
  return a:value
endfunction

function Env.get(key) dict
  let curr = self
  while !has_key(curr.data, a:key)
    let curr = curr.outer
    if empty(curr)
      return ""
    endif
  endwhile
  return curr.data[a:key]
endfunction

function Env.root() dict
  let curr = self
  while !empty(curr.outer)
    let curr = curr.outer
  endwhile
  return curr
endfunction
