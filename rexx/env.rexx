#ifndef __env__
#define __env__

env. = ""
env.0 = 0

new_env_index: procedure expose env. /* new_env_index() */
  env.0 = env.0 + 1
  return env.0

new_env: procedure expose env. values. /* new_env(outer_env_idx [, binds, exprs]) */
  outer_env_idx = arg(1)
  binds = arg(2)
  exprs = arg(3)
  idx = new_env_index()
  env.idx.outer = outer_env_idx
  env.idx.data. = ""
  if binds \= "" then do
    binds_val = obj_val(binds)
    exprs_val = obj_val(exprs)
    do i=1 to words(binds_val)
      varname = obj_val(word(binds_val, i))
      if varname == "&" then do
        rest_args_list = new_list(subword(exprs_val, i))
        varname = obj_val(word(binds_val, i + 1))
        x = env_set(idx, varname, rest_args_list)
        leave
      end
      else
        x = env_set(idx, varname, word(exprs_val, i))
    end
  end
  return idx

env_set: procedure expose env. /* env_set(env_idx, key, val) */
  env_idx = arg(1)
  key = arg(2)
  val = arg(3)
  env.env_idx.data.key = val
  return val

env_find: procedure expose env. /* env_find(env_idx, key) */
  env_idx = arg(1)
  key = arg(2)
  if env.env_idx.data.key \= "" then return env_idx
  if env.env_idx.outer > 0 then return env_find(env.env_idx.outer, key)
  return 0

env_get: procedure expose env. err /* env_get(env_idx, key) */
  env_idx = arg(1)
  key = arg(2)
  found_env_idx = env_find(env_idx, key)
  if found_env_idx == 0 then do
    err = "'" || key || "' not found"
    return "ERR"
  end
  return env.found_env_idx.data.key

#endif
