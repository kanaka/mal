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

  Val get(string key)
  {
    Val res = data[key];
    if(res) return res;
    if(outer) return outer.get(key);
    return 0;
  }
}
