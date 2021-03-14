using System;
using System.Collections.Generic;

namespace mal
{
    public class MalType
    {
    }

    public class MalList : MalType
    {
        public IList<MalType> items { get; }
        public string openingBracket { get; }

        public MalList(IList<MalType> items, string openingBracket = "(")
        {
            this.items = items;
            this.openingBracket = openingBracket;
        }

        public bool isList()
        {
            return this.openingBracket == "(";
        }
    }

    class MalInteger : MalType
    {
        public int value { get; }
        public MalInteger(int value)
        {
            this.value = value;
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
    }

    class MalString : MalType
    {
        public string value { get; }

        public MalString(string value)
        {
            this.value = value;
        }
    }

    class MalHashmap : MalType
    {
        public Dictionary<MalType, MalType> values { get; }

        public MalHashmap(Dictionary<MalType, MalType> values)
        {
            this.values = values;
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
    }
}