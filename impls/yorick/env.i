require, "hash.i"
require, "types.i"

struct Env {
  pointer outer
  Hash data
}

func env_new(outer_ptr, binds=, exprs=)
{
  env = Env(outer=outer_ptr, data=hash_new())
  for (i = 1; i <= numberof(binds); ++i) {
    if (binds(i)->val == "&") {
      rest_args = numberof(exprs) >= i ? exprs(i:) : []
      env_set, env, binds(i + 1)->val, MalList(val=&rest_args)
      break
    } else {
      env_set, env, binds(i)->val, *exprs(i)
    }
  }
  return env
}

func env_find(env, key)
{
  if (hash_has_key(env.data, key)) return env
  if (is_void(*env.outer)) return nil
  return env_find(*env.outer, key)
}

func env_get(env, key)
{
  found_env = env_find(env, key)
  if (is_void(found_env)) return MalError(message=("'" + key + "' not found"))
  return hash_get(found_env.data, key)
}

func env_set(&env, key, val)
{
  d = env.data
  hash_set, d, key, val
  env.data = d
  return val
}
