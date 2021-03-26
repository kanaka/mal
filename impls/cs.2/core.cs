using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

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

            {"nth",
                new MalFunction((IList<MalType> args) => {
                    MalSeq seq = (MalSeq)args[0];
                    MalInteger index = (MalInteger)args[1];
                    return seq.items[index.value];
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

        };
    }
}