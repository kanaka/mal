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
}