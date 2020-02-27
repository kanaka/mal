import .Types;

class Env
{
  Env outer;
  mapping(string:Val) data;

  void create(Env the_outer, List|void binds, List|void exprs)
  {
    outer = the_outer;
    data = ([ ]);
    if(binds)
    {
      for(int i = 0; i < binds.count(); i++)
      {
        if(binds.data[i].value == "&")
        {
          set(binds.data[i + 1], List(exprs.data[i..]));
          break;
        }
        set(binds.data[i], exprs.data[i]);
      }
    }
  }

  Val set(Val key, Val val)
  {
    data[key.value] = val;
    return val;
  }

  Env find(Val key)
  {
    if(data[key.value]) return this_object();
    if(outer) return outer.find(key);
    return 0;
  }

  Val get(Val key)
  {
    Env found_env = find(key);
    if(!found_env) throw("'" + key.value + "' not found");
    return found_env.data[key.value];
  }
}
