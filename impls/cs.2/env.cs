using System;
using System.Collections.Generic;
using System.Linq;

namespace mal
{
    class Env
    {
        public Env outer { get; }
        public Dictionary<MalSymbol, MalType> data { get; }
        public Env(Env outer, IList<MalSymbol> binds = null, IList<MalType> exprs = null)
        {
            this.outer = outer;
            this.data = new Dictionary<MalSymbol, MalType>();
            if (binds != null && exprs != null)
            {
                for (int i = 0; i < binds.Count; i++)
                {
                    MalSymbol key = binds[i];

                    // If the key is the symbol '&', collect the rest of the exprs in a list and bind it to the next binds
                    if (key.value == "&")
                    {
                        MalSymbol nextKey = binds[i + 1];
                        List<MalType> remaining = exprs.Skip(i).ToList();
                        data.Add(nextKey, new MalList(remaining));
                        return;
                    }
                    else
                    {
                        if (data.ContainsKey(key)) { data.Remove(key); }
                        MalType value = exprs[i];
                        data.Add(key, value);
                    }
                }
            }
        }

        public void set(MalSymbol key, MalType value)
        {
            if (data.ContainsKey(key)) { data.Remove(key); }
            this.data.Add(key, value);
        }

        public MalType find(MalSymbol key)
        {
            return this.data.GetValueOrDefault(key, outer?.find(key));
        }

        public MalType get(MalSymbol key)
        {
            MalType found = find(key);
            if (found != null)
            {
                return found;
            }
            else
            {
                throw new MalException(new MalString(string.Format("'{0}' not found", key.value)));
            }
        }

    }
}