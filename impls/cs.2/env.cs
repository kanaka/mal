using System;
using System.Collections.Generic;

namespace mal
{
    class Env
    {
        public Env outer { get; }
        public Dictionary<MalSymbol, MalType> data { get; }
        public Env(Env outer)
        {
            this.outer = outer;
            this.data = new Dictionary<MalSymbol, MalType>();
        }

        public void set(MalSymbol key, MalType value)
        {
            if (data.ContainsKey(key))
            {
                data.Remove(key);
            }
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
                throw new Exception(string.Format("Value for '{0}' not found", key.value));
            }
        }

    }
}