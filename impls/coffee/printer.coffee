types = require "./types.coffee"

exports.println = (args...) -> console.log(args.join(" ")) || null

exports._pr_str = _pr_str = (obj, print_readably=true) ->
  _r = print_readably
  switch types._obj_type obj
    when 'list' then '(' + obj.map((e) -> _pr_str(e,_r)).join(' ') + ')'
    when 'vector' then '[' + obj.map((e) -> _pr_str(e,_r)).join(' ') + ']'
    when 'hash-map'
      ret = []
      ret.push(_pr_str(k,_r), _pr_str(v,_r)) for k,v of obj
      '{' + ret.join(' ') + '}'
    when 'string'
      if _r then '"' + (obj.replace(/\\/g, '\\\\')
                           .replace(/"/g, '\\"')
                           .replace(/\n/g, '\\n')) + '"'
      else obj
    when 'keyword' then ":" + obj.slice(1)
    when 'symbol' then obj.name
    when 'nil' then 'nil'
    when 'atom' then "(atom " + _pr_str(obj.val,_r) + ")"
    else obj.toString()
    
# vim: ts=2:sw=2
