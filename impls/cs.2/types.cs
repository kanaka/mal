using System;
using System.Collections.Generic;

namespace mal
{
    public class MalType { }

    public class MalSeq : MalType
    {
        public IList<MalType> items { get; }
        public string openingBracket { get; }
        public MalSeq(IList<MalType> items, string openingBracket)
        {
            this.items = items;
            this.openingBracket = openingBracket;
        }

        public override string ToString()
        {
            return string.Format("<{0} {1}>", this.GetType().Name, items.ToString());
        }

        public override int GetHashCode()
        {
            return items.GetHashCode();
        }

        public override bool Equals(object other)
        {
            if (other is not MalSeq) return false;
            IList<MalType> otherItems = ((MalSeq)other).items;
            if (otherItems.Count != items.Count) return false;
            for (int i = 0; i < items.Count; i++)
            {
                if (!items[i].Equals(otherItems[i])) return false;
            }
            return true;
        }
    }

    public class MalVector : MalSeq
    {
        public MalVector(IList<MalType> items) : base(items, "[")
        {
        }
    }

    public class MalList : MalSeq
    {
        public MalList(IList<MalType> items) : base(items, "(")
        {
        }
    }

    class MalInteger : MalType
    {
        public int value { get; }
        public MalInteger(int value)
        {
            this.value = value;
        }

        public override string ToString()
        {
            return string.Format("<MalInteger {0}>", value.ToString());
        }

        public override int GetHashCode()
        {
            return value.GetHashCode();
        }

        public override bool Equals(object other)
        {
            return ((other is MalInteger) && ((MalInteger)other).value == value);
        }
    }

    class MalSymbol : MalType
    {
        public string value { get; }

        public MalSymbol(string value)
        {
            this.value = value;
        }

        public override int GetHashCode()
        {
            return value.GetHashCode();
        }

        public override bool Equals(object other)
        {
            return ((other is MalSymbol) && ((MalSymbol)other).value == value);
        }

        public override string ToString()
        {
            return string.Format("<MalSymbol {0}>", value.ToString());
        }
    }

    class MalString : MalType
    {
        public string value { get; }

        public MalString(string value)
        {
            this.value = value;
        }

        public override string ToString()
        {
            return string.Format("<MalString {0}>", value.ToString());
        }

        public override int GetHashCode()
        {
            return value.GetHashCode();
        }

        public override bool Equals(object other)
        {
            return ((other is MalString) && ((MalString)other).value == value);
        }
    }

    class MalHashmap : MalType
    {
        public Dictionary<MalType, MalType> values { get; }

        public MalHashmap(Dictionary<MalType, MalType> values) { this.values = values; }

        public override int GetHashCode() { return values.GetHashCode(); }

        public override bool Equals(object other)
        {
            if (other is not MalHashmap) return false;
            MalHashmap otherMap = (MalHashmap)other;
            if (otherMap.values.Count != values.Count) return false;
            foreach (var kv in values)
            {
                MalType key = kv.Key;
                if (values.GetValueOrDefault(key) != otherMap.values.GetValueOrDefault(key)) return false;
            }
            return true;
        }
    }

    class MalFunction : MalType
    {
        public Func<IList<MalType>, MalType> function { get; }
        public MalFunction(Func<IList<MalType>, MalType> function)
        {
            this.function = function;
        }
    }

    class MalKeyword : MalType
    {
        public string name { get; }

        public MalKeyword(string name)
        {
            this.name = name;
        }

        public override int GetHashCode()
        {
            return name.GetHashCode();
        }

        public override bool Equals(object other)
        {
            return ((other is MalKeyword) && ((MalKeyword)other).name == name);
        }

        public override string ToString()
        {
            return string.Format("<MalKeyword {0}>", name.ToString());
        }
    }

    class MalNil : MalType
    {
        public static MalNil MAL_NIL = new MalNil();

        private MalNil() { }
        public override string ToString()
        {
            return "<MalNil>";
        }
    }

    class MalBoolean : MalType
    {
        public static MalBoolean MAL_TRUE = new MalBoolean(true);
        public static MalBoolean MAL_FALSE = new MalBoolean(false);
        public bool value { get; }

        MalBoolean(bool value)
        {
            this.value = value;
        }
        public override string ToString()
        {
            return string.Format("<MalBoolean {0}>", value.ToString());
        }
    }

    class MalFnTco : MalType
    {
        public MalType ast { get; set; }
        public List<MalSymbol> @params { get; set; } // 'params' is a reserved word
        public Env env { get; set; }
        public MalFunction fn { get; set; }

        public MalFnTco(MalType ast, List<MalSymbol> @params, Env env, MalFunction fn)
        {
            this.ast = ast;
            this.@params = @params;
            this.env = env;
            this.fn = fn;
        }
    }

    class MalAtom : MalType
    {
        public MalType value { get; set; }

        public MalAtom(MalType value)
        {
            this.value = value;
        }
    }
}