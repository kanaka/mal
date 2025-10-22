using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace mal
{
    class core
    {
        public static Dictionary<string, MalFunction> ns = new Dictionary<string, MalFunction>()
        {
            {"list", new MalFunction((IList<MalType> args) => new MalList(args)) },
            {"list?", new MalFunction((IList<MalType> args) => (args[0] is MalList) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE)},
            {"empty?", new MalFunction((IList<MalType> args) =>
                (args[0] is MalSeq && ((MalSeq)args[0]).items.Count == 0) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE
            )},
            {"count", new MalFunction((IList<MalType> args) => {
                if (args.Count == 0 || args[0] == MalNil.MAL_NIL) return new MalInteger(0);
                if (args[0] is MalHashmap) return new MalInteger( ((MalHashmap)args[0]).values.Count );
                else return new MalInteger( ((MalSeq)args[0]).items.Count );
            })},
            {"+", new MalFunction((IList<MalType> args) => new MalInteger(((MalInteger)args[0]).value + ((MalInteger)args[1]).value))},
            {"-", new MalFunction((IList<MalType> args) => new MalInteger(((MalInteger)args[0]).value - ((MalInteger)args[1]).value))},
            {"*", new MalFunction((IList<MalType> args) => new MalInteger(((MalInteger)args[0]).value * ((MalInteger)args[1]).value))},
            {"/", new MalFunction((IList<MalType> args) => new MalInteger(((MalInteger)args[0]).value / ((MalInteger)args[1]).value))},

            {"<", new MalFunction((IList<MalType> args) => (((MalInteger)args[0]).value < ((MalInteger)args[1]).value) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE )},
            {"<=", new MalFunction((IList<MalType> args) => (((MalInteger)args[0]).value <= ((MalInteger)args[1]).value) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE )},
            {">", new MalFunction((IList<MalType> args) => (((MalInteger)args[0]).value > ((MalInteger)args[1]).value) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE )},
            {">=", new MalFunction((IList<MalType> args) => (((MalInteger)args[0]).value >= ((MalInteger)args[1]).value) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE )},

            {"=", new MalFunction((IList<MalType> args) => (args[0].Equals(args[1])) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE)},

            // string functions
            {"pr-str",
            new MalFunction((IList<MalType> args) => {
                    List<string> printed = args.Select(it => printer.pr_str(it, true)).ToList();
                    return new MalString(string.Join(" ", printed));
                })},
            {"str",
            new MalFunction((IList<MalType> args) => {
                    List<string> printed = args.Select(it => printer.pr_str(it, false)).ToList();
                    return new MalString(string.Join("", printed));
                })},
            {"prn",
            new MalFunction((IList<MalType> args) => {
                    List<string> printed = args.Select(it => printer.pr_str(it, true)).ToList();
                    Console.WriteLine(string.Join(" ", printed));
                    return MalNil.MAL_NIL;
                })},
            {"println",
            new MalFunction((IList<MalType> args) => {
                    List<string> printed = args.Select(it => printer.pr_str(it, false)).ToList();
                    Console.WriteLine(string.Join(" ", printed));
                    return MalNil.MAL_NIL;
                })},

            // Step 6
            {"read-string",
                new MalFunction((IList<MalType> args) => {
                    MalString arg = (MalString)args[0];
                    return Reader.read_str(arg.value);
                })},

            {"slurp",
                new MalFunction((IList<MalType> args) => {
                    MalString malFilename = (MalString)args[0];
                    string path = malFilename.value;
                    string contents = File.ReadAllText(path);
                    return new MalString(contents);
                })},

            {"atom", new MalFunction((IList<MalType> args) => { return new MalAtom(args[0]); })},

            {"atom?", new MalFunction((IList<MalType> args) => {
                return (args[0] is MalAtom) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE;
                })},

            {"deref", new MalFunction((IList<MalType> args) => { return ((MalAtom)args[0]).value; })},

            {"reset!", new MalFunction((IList<MalType> args) => {
                MalAtom atom = (MalAtom)args[0];
                MalType newValue = args[1];
                atom.value = newValue;
                return newValue;
            })},

            {"swap!", new MalFunction((IList<MalType> args) => {
                MalAtom atom = (MalAtom)args[0];
                List<MalType> fnArgs = new List<MalType>(){ atom.value };
                fnArgs.AddRange(args.Skip(2).ToList());
                MalType fnOrFnTco = args[1];
                MalType newValue;
                if (fnOrFnTco is MalFunction) {
                    newValue = ((MalFunction)fnOrFnTco).function(fnArgs);
                } else {
                    newValue = ((MalFnTco)fnOrFnTco).fn.function(fnArgs);
                }
                atom.value = newValue;
                return newValue;
            })},

            // Step 7
            {"cons",
                new MalFunction((IList<MalType> args) => {
                    MalType head = args[0];
                    MalSeq rest = (MalSeq)args[1];
                    List<MalType> newList = new List<MalType>(){ head };
                    newList.AddRange(rest.items);
                    return new MalList(newList);
                })},

            {"concat",
                new MalFunction((IList<MalType> args) => {
                    List<MalType> newList = new List<MalType>();
                    foreach (MalType arg in args)
                    {
                        if (arg is MalSeq)
                        {
                            MalSeq argList = (MalSeq)arg;
                            newList.AddRange(argList.items);
                        }
                    }
                    return new MalList(newList);
                })},

            {"vec",
                new MalFunction((IList<MalType> args) => {
                    MalSeq head = (MalSeq)args[0];
                    if (head is MalVector) return head;
                    return new MalVector(head.items);
                })},

            // Step 8

            {"nth",
                new MalFunction((IList<MalType> args) => {
                    MalSeq seq = (MalSeq)args[0];
                    MalInteger index = (MalInteger)args[1];
                    if (index.value > seq.items.Count - 1) throw new MalException(new MalString("index out of bounds"));
                    return seq.items[(int)index.value];
                })},

            {"first",
                new MalFunction((IList<MalType> args) => {
                    if (args[0] == MalNil.MAL_NIL) return MalNil.MAL_NIL;
                    MalSeq seq = (MalSeq)args[0];
                    if (seq == null || seq.items.Count == 0) return MalNil.MAL_NIL;
                    return seq.items[0];
                })},

            {"rest",
                new MalFunction((IList<MalType> args) => {
                    if (args[0] == MalNil.MAL_NIL) return new MalList(new List<MalType>());
                    MalSeq seq = (MalSeq)args[0];
                    if (args[0] == MalNil.MAL_NIL || seq == null || seq.items.Count == 0) return new MalList(new List<MalType>());
                    return new MalList( seq.items.Skip(1).ToList() );
                })},

            // Step 9

            {"throw",
                new MalFunction((IList<MalType> args) => {
                    throw new MalException(args[0]);
                })},

            {"apply",
                new MalFunction((IList<MalType> args) => {
                    MalType fnOrTco = args[0];
                    Func<IList<MalType>, MalType> fn = (fnOrTco is MalFunction) ? ((MalFunction)fnOrTco).function : ((MalFnTco)fnOrTco).fn.function;
                    List<MalType> fnArgs = args.Skip(1).ToList();
                    if (args[args.Count - 1] is MalSeq)
                    {
                        MalSeq lastElem = (MalSeq)args[args.Count - 1];
                        fnArgs = args.Skip(1).Take(args.Count - 2).ToList(); // all but first and last
                        fnArgs.AddRange( lastElem.items ); // add all elements of the last item
                    }
                    return fn(fnArgs);
                })},

            {"map",
                new MalFunction((IList<MalType> args) => {
                    MalType fnOrTco = args[0];
                    Func<IList<MalType>, MalType> fn = (fnOrTco is MalFunction) ? ((MalFunction)fnOrTco).function : ((MalFnTco)fnOrTco).fn.function;
                    MalSeq xs = (MalSeq)args[1];
                    List<MalType> mapped = xs.items.Select(x => fn( new List<MalType>(){x} )).ToList();
                    return new MalList(mapped);
                })},

            {"symbol",
                new MalFunction((IList<MalType> args) => {
                    MalString name = (MalString)args[0];
                    return new MalSymbol(name.value);
                })},

            {"symbol?",
                new MalFunction((IList<MalType> args) => {
                    MalType x = args[0];
                    return (x is MalSymbol) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE;
                })},

            {"keyword",
                new MalFunction((IList<MalType> args) => {
                    MalType name = args[0];
                    return new MalKeyword(printer.pr_str(name));
                })},

            {"keyword?",
                new MalFunction((IList<MalType> args) => {
                    MalType x = args[0];
                    return (x is MalKeyword) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE;
                })},

            {"vector",
                new MalFunction((IList<MalType> args) => {
                    return new MalVector(args);
                })},

            {"vector?",
                new MalFunction((IList<MalType> args) => {
                    MalType x = args[0];
                    return (x is MalVector) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE;
                })},

            {"sequential?",
                new MalFunction((IList<MalType> args) => {
                    MalType x = args[0];
                    return (x is MalSeq) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE;
                })},

            {"hash-map",
                new MalFunction((IList<MalType> args) => {
                    Dictionary<MalType, MalType> dict = new Dictionary<MalType, MalType>();
                    for (int i = 0; i < args.Count; i += 2)
                    {
                        MalType key = args[i];
                        MalType val = args[i+1];
                        if (dict.ContainsKey(key)) dict.Remove(key);
                        dict.Add(key, val);
                    }
                    return new MalHashmap( dict );
                })},

            {"map?",
                new MalFunction((IList<MalType> args) => {
                    MalType x = args[0];
                    return (x is MalHashmap) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE;
                })},

            {"assoc",
                new MalFunction((IList<MalType> args) => {
                    MalHashmap baseMap = (MalHashmap)args[0];
                    Dictionary<MalType, MalType> dict = new Dictionary<MalType, MalType>(baseMap.values);
                    for (int i = 1; i < args.Count; i += 2)
                    {
                        MalType key = args[i];
                        MalType val = args[i+1];
                        if (dict.ContainsKey(key)) dict.Remove(key);
                        dict.Add(key, val);
                    }
                    return new MalHashmap( dict );
                })},

            {"dissoc",
                new MalFunction((IList<MalType> args) => {
                    MalHashmap baseMap = (MalHashmap)args[0];
                    Dictionary<MalType, MalType> dict = new Dictionary<MalType, MalType>(baseMap.values);
                    for (int i = 1; i < args.Count; i++)
                    {
                        MalType key = args[i];
                        if (dict.ContainsKey(key)) dict.Remove(key);
                    }
                    return new MalHashmap( dict );
                })},

            {"get",
                new MalFunction((IList<MalType> args) => {
                    if (args[0] is not MalHashmap) return MalNil.MAL_NIL;
                    MalHashmap m = (MalHashmap)args[0];
                    MalType k = args[1];
                    return (m.values.ContainsKey(k)) ? m.values.GetValueOrDefault(k) : MalNil.MAL_NIL;
                })},

            {"contains?",
                new MalFunction((IList<MalType> args) => {
                    MalHashmap m = (MalHashmap)args[0];
                    MalType k = args[1];
                    return (m.values.ContainsKey(k)) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE;
                })},

            {"keys",
                new MalFunction((IList<MalType> args) => {
                    MalHashmap m = (MalHashmap)args[0];
                    return new MalList( m.values.Keys.ToList() );
                })},

            {"vals",
                new MalFunction((IList<MalType> args) => {
                    MalHashmap m = (MalHashmap)args[0];
                    return new MalList( m.values.Values.ToList() );
                })},

            {"nil?",
                new MalFunction((IList<MalType> args) => {
                    MalType x = args[0];
                    return (x == MalNil.MAL_NIL) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE;
                })},

            {"true?",
                new MalFunction((IList<MalType> args) => {
                    MalType x = args[0];
                    return (x == MalBoolean.MAL_TRUE) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE;
                })},

            {"false?",
                new MalFunction((IList<MalType> args) => {
                    MalType x = args[0];
                    return (x == MalBoolean.MAL_FALSE) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE;
                })},

            // Step A
            {"readline",
                new MalFunction((IList<MalType> args) => {
                    Console.Write(printer.pr_str(args[0]));
                    string line = Console.ReadLine();
                    return (line == null) ? MalNil.MAL_NIL : new MalString(line);
                })},

            {"time-ms",
                new MalFunction((IList<MalType> args) => {
                    return new MalInteger(DateTimeOffset.Now.ToUnixTimeMilliseconds());
                })},

            {"conj",
                new MalFunction((IList<MalType> args) => {
                    MalSeq coll = (MalSeq)args[0];
                    // MalType elem = args[1];
                    List<MalType> items = new List<MalType>();
                    if (coll is MalList)
                    {
                        items.AddRange(args.Skip(1).Reverse().ToList());
                        items.AddRange(coll.items);
                    }
                    else
                    {
                        items.AddRange(coll.items);
                        items.AddRange(args.Skip(1).ToList());
                    }
                    return (coll is MalList) ? new MalList(items) : new MalVector(items);
                })},

            {"string?",
                new MalFunction((IList<MalType> args) => {
                    return (args[0] is MalString) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE;
                })},

            {"number?",
                new MalFunction((IList<MalType> args) => {
                    return (args[0] is MalInteger) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE;
                })},

            {"fn?",
                new MalFunction((IList<MalType> args) => {
                    if (args[0] is MalFunction && !((MalFunction)args[0]).is_macro) return MalBoolean.MAL_TRUE;
                    if (args[0] is MalFnTco && !((MalFnTco)args[0]).is_macro) return MalBoolean.MAL_TRUE;
                    else return MalBoolean.MAL_FALSE;
                })},

            {"macro?",
                new MalFunction((IList<MalType> args) => {
                    if (args[0] is MalFunction && ((MalFunction)args[0]).is_macro) return MalBoolean.MAL_TRUE;
                    if (args[0] is MalFnTco && ((MalFnTco)args[0]).is_macro) return MalBoolean.MAL_TRUE;
                    else return MalBoolean.MAL_FALSE;
                })},

            {"seq",
                new MalFunction((IList<MalType> args) => {
                    if (args[0] is MalList && ((MalList)args[0]).items.Count > 0) return args[0];
                    if (args[0] is MalVector && ((MalVector)args[0]).items.Count > 0) return new MalList( ((MalVector)args[0]).items );
                    if (args[0] is MalString && ((MalString)args[0]).value.Length > 0)
                    {
                        string value = ((MalString)args[0]).value;
                        List<MalType> strings = new List<MalType>();
                        strings.AddRange (value.ToCharArray().Select(c => new MalString(c.ToString())).ToList() );
                        return new MalList(strings);
                    }
                    else return MalNil.MAL_NIL;
                })},

            {"with-meta",
                new MalFunction((IList<MalType> args) => {
                    MalType arg = args[0];
                    MalType meta = args[1];
                    if (arg is MalMeta) {
                        MalMeta orig = (MalMeta)arg;
                        MalMeta clone = (MalMeta)(orig.Clone());
                        clone.meta = meta;
                        return clone;
                    }
                    return arg;
                })},

            {"meta",
                new MalFunction((IList<MalType> args) => {
                    if (args[0] is MalMeta)
                    {
                        MalMeta mm = (MalMeta)args[0];
                        if (mm.meta != null) return mm.meta;
                    }
                    return MalNil.MAL_NIL;
                })},

        };
    }
}